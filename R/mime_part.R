.mime_part_finalizer <- function(x) {
  if (!is.null(x$file))
    file.remove(x$file)
}

.mime_part <- function(headers, file=NULL, text=NULL) {
  if (!is.null(file) && !is.null(text))
    stop("Can only provide file or text for mime part.")

  e <- environment()
  reg.finalizer(e, .mime_part_finalizer, onexit=TRUE)
  class(e) <- "mime_part"
  e
}

.write_mime_part <- function(mp, con=stdout()) {
  writeLines(paste(names(mp$headers), unlist(mp$headers), sep=": "),
             con, sep="\r\n")
  writeLines("", con, sep="\r\n")
  if (is.null(mp$file))
    writeLines(mp$text, con)
  else
    writeLines(readLines(mp$file), con, sep="\r\n")
}

#' @importFrom base64enc base64encode
.file_attachment <- function(fn, name,
                             type="application/octet-stream",
                             disposition="attachment") {
  if (missing(name))
    name <- basename(fn)

  text <- base64enc::base64encode(fn, linewidth=72, newline="\n")
  headers <- list("Content-Type"=type,
                  "Content-Disposition"=sprintf("%s; filename=%s",
                    disposition, name),
                  "Content-Transfer-Encoding"="base64")

  .mime_part(headers=headers, text=text)
}

.plot_attachment <- function(plt, name=deparse(substitute(plt)), device, ...) {
  fn <- tempfile()
  device(file=fn, ...)
  print(plt)
  grDevices::dev.off()
  ## FIXME: Guess content type from device!
  res <- .file_attachment(fn, name, type="application/pdf")
  file.remove(fn)
  res
}

.generate_charset_convert_utf8 <- function(x) {

  e <- Encoding(x)

  # 1 If all character strings are valid utf-8 set charset utf-8
  if (all(validUTF8(x))) {
    result <- list(x = x,
               charset = "; charset=utf-8")
    return(result)
  }

  # 2 If there is any non utf-8 encoded text, convert to utf-8
  if (any(e != "unknown")) {
    result <- list(x = enc2utf8(x),
                   charset = "; charset=utf-8")
    return(result)
  }

  # 3 Default content_type (backward compatibility)
  result <- list(x = x,
                 charset = "")
  return(result)
}

##' Create a MIME part
##'
##' @param x Object to include
##' @param name Name of mime part. Usually the filename of the
##'   attachment as displayed by the e-mail client.
##' @param ... Possible further arguments for \code{mime_part}
##'   implementations.
##' @return An S3 \code{mime_part} object.
##' @seealso \code{\link{mime_part.character}}, \code{\link{mime_part_html}}, 
##' \code{\link{mime_part.data.frame}}, \code{\link{mime_part.matrix}}, 
##' \code{\link{mime_part.ggplot}}, \code{\link{mime_part.trellis}}
##' 
##' @export
mime_part <- function(x, name, ...)
  UseMethod("mime_part", x)

##' Default MIME part method
##'
##' Creates a string representation of the object \code{x} using
##' \code{dput}. This representation is then turned into a file
##' attachment.
##'
##' @param x R object
##' @param name Filename used for attachment (sans the .R extension)
##' @param ... Ignored.
##' @return An S3 \code{mime_part} object.
##'
##' @method mime_part default
##' @export
mime_part.default <- function(x, name, ...) {
  str <- dput(x)
  .mime_part(headers=list(
               "Content-Type"="text/plain",
               "Content-Disposition"=sprintf("attachment; file=%s.R", name)),
             text=str)
}

##' Creates a MIME part from a trellis plot object
##'
##' Writes a PDF file of the plot defined by \code{x} and turns this
##' PDF file into a file attachment.
##'
##' @param x A \code{trellis} (lattice) object
##' @param name Name of attachment (sans .pdf extension).
##' @param device Graphics device used to render the plot. Defaults to
##'   \code{pdf}.
##' @param ... Ignored.
##' @return An S3 \code{mime_part} object.

##' @importFrom grDevices pdf
##' @method mime_part trellis
##' @export
mime_part.trellis <- function(x, name=deparse(substitute(x)), device = pdf, ...)
  .plot_attachment(x, name=name, device=device, ...)

##' Creates a MIME part from a ggplot2 plot object
##'
##' Writes a PDF file of the plot defined by \code{x} and turns this
##' PDF file into a file attachment.
##'
##' @param x A \code{ggplot} object
##' @param name Name of attachment (sans .pdf extension).
##' @param device Graphics device used to render the plot. Defaults to
##'   \code{pdf}.
##' @param ... Ignored.
##' @return An S3 \code{mime_part} object.
##'
##' @importFrom grDevices pdf
##' @method mime_part ggplot
##' @export
mime_part.ggplot <- function(x, name=deparse(substitute(x)), device = pdf, ...)
  .plot_attachment(x, name=name, device=device, ...)

##' Create a MIME part from a matrix.
##'
##' @param x Matrix
##' @param name Basename of file attachment that is generated.
##' @param ... Ignored.
##' @return An S3 \code{mime_part} object
##'
##' @method mime_part matrix
##' @export
mime_part.matrix <- function(x, name=deparse(substitute(x)), ...) {
  f <- tempfile()
  on.exit(file.remove(f))
  utils::write.table(x, file=f, ...)
  .file_attachment(f, name=sprintf("%s.txt", name), type="text/plain")
}

##' Create a MIME part from a \code{data.frame}.
##'
##' @param x A \code{data.frame}.
##' @param name Basename of file attachment that is generated.
##' @param filename_extension Filename extension (i.e., the suffix) to be used
##'   for the attached file.
##' @param ... Ignored.
##' @return An S3 \code{mime_part} object.
##'
##' @method mime_part data.frame
##' @export
mime_part.data.frame <- function(
  x
  , name = deparse(substitute(x))
  , filename_extension = ".txt"
  , ...
) {

  f <- tempfile()
  on.exit(file.remove(f))

  utils::write.table(x, file = f, ...)

  name <- sprintf("%s%s", name, filename_extension)
  .file_attachment(f, name = name, type = "text/plain")
}

##' Create a MIME part from a character string. If the string matches
##' a filename, a MIME part containing that file is returned instead.
##'
##' @title Create an inline character MIME Part
##'
##' @param x Character string, possibly a filename.
##' @param name Name of attachment.
##' @param type Content type of inline text. Defaults to "text/plain".
##' @param flowed Should "format=flowed" be added to the content header.
##' @param ... Ignored.
##' @return An S3 \code{mime_part} object.
##'
##' @method mime_part character
##' @export
##' @seealso \code{\link{mime_part_html}} for adding inline HTML
mime_part.character <- function(x, name, type = "text/plain", flowed = FALSE, ...) {
  if (length(x) == 1 && file.exists(x)) {
    .file_attachment(x, name, ...)
  } else {

    res <- .generate_charset_convert_utf8(x)
    format_flowed <- ifelse(flowed, "; format=flowed", "")

    # e.g. Content-Type: text/plain; charset=utf-8
    content_type <- paste0(type, res$charset, format_flowed)

    .mime_part(headers = list(
               "Content-Type" = content_type,
               "Content-Disposition" = "inline"),
             text = paste(res$x, collapse = "\r\n"))
  }
}

##' Create a MIME part from a character string containing HTML. If the string matches
##' a filename the file is read and inserted as an inline character MIME part.
##'
##' @title Create an inline HTML MIME Part
##'
##' @param x Character string, vector/list of character strings
##'   or path to html file.
##' @param ... Ignored.
##' @return An S3 \code{mime_part} object.
##'
##' @examples
##' \dontrun{
##' sendmail(
##'   from="from@example.org",
##'   to="to1@example.org",
##'   subject="inline HTML",
##'   msg=mime_part_html("Hello<br>World"),
##'   control=list(smtpServer="ASPMX.L.GOOGLE.COM")
##' )
##'
##' sendmail(
##'   from="from@example.org",
##'   to="to1@example.org",
##'   subject="inline HTML",
##'   msg=mime_part_html("out/report.html"),
##'   control=list(smtpServer="ASPMX.L.GOOGLE.COM")
##' )
##' }
##'
##' @export
mime_part_html <- function(x, ...) {

  if (length(x) == 1 && file.exists(x)) {
    x <- readLines(x)
  }

  # For compatibility with xml2::read_html()
  if (is.list(x)) x <- as.character(x)

  mime_part.character(x, type = "text/html")
}
