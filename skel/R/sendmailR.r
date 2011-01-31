##
## sendmailR.r - send email from within R
##
## Author:
##  Olaf Mersmann (OME) <olafm@datensplitter.net>
##

.write_mail <- function(headers, msg, sock) {
  if (!is.list(msg))
    msg <- list(msg)

  ## Generate MIME headers:
  boundary <- paste(packBits(sample(0:1, 256, TRUE)), collapse="")
  headers$`MIME-Version` <- "1.0"
  headers$`Content-Type` <- sprintf("multipart/mixed; boundary=\"%s\"", boundary)

  writeLines(paste(names(headers),
                   unlist(headers), sep=": "),
             sock, sep="\r\n")
  writeLines("", sock, sep="\r\n")

  writeLines("This is a message with multiple parts in MIME format.", sock, sep="\r\n")

  for (part in msg) {
    writeLines(sprintf("--%s", boundary), sock, sep="\r\n")
    if (inherits(part, "mime_part"))
      .write_mime_part(part, sock)
    else if (is.character(part)) { ## Legacy support for plain old string
      ## writeLines(sprintf("--%s", boundary), sock, sep="\r\n")
      writeLines("Content-Type: text/plain; format=flowed\r\n", sock, sep="\r\n")
      writeLines(part, sock, sep="\r\n")
    }
  }
  writeLines(sprintf("--%s--", boundary), sock, sep="\r\n")
}

.smtp_submit_mail <- function(server, port, headers, msg, verbose=FALSE) {
  stopifnot(is.character(headers$From), is.character(headers$To))
  
  wait_for <- function(lcode) {
    done <- FALSE
    while (!done) {
      line <- readLines(con=sock, n=1)
      if (verbose)
        message("<< ", line)
      code <- substring(line, 1, 3)
      msg <- substring(line, 5)
      if (code == lcode) {
        done <- TRUE
      } else {
        if (code >= 500 & code <= 599)
          stop("SMTP Error: ", msg)
        else
          message("Unknown SMTP code: ", code)
      }
      
    }
    return(list(code=code, msg=msg))
  }

  send_command <- function(cmd, code) {
    if (verbose)
      message(">> ", cmd)
    writeLines(cmd, sock, sep="\r\n")
    wait_for(code)
  }

  nodename <- Sys.info()[4]
  sock <- socketConnection(host=server,
                           port=port,
                           blocking=TRUE)
  if (!isOpen(sock))
    stop(sprintf("Could not connect to smtp server '%s' on port '%i'.",
                 server, port))
  on.exit(close(sock))
  ## << 220 <hostname> ESMTP
  wait_for(220)
  ## >> HELO localhost
  ## << 250 mail.statistik.uni-dortmund.de
  send_command(paste("HELO ", nodename), 250)
  ## >> MAIL FROM: <foo@bah.com>
  ## << 250 2.1.0 Ok
  send_command(paste("MAIL FROM: ", headers$From), 250)
  ## >> RCPT TO: <bah@baz.org>
  ## << 250 2.1.5 Ok
  send_command(paste("RCPT TO: ", headers$To), 250)
  ## >> DATA
  ## << 354 blah fu
  send_command("DATA", 354)
  ## >> <actual message + headers + .>
  if (verbose)
    message(">> <message data>")

  .write_mail(headers, msg, sock)
  
  writeLines(".", sock, sep="\r\n")
  
  wait_for(250)
  ## << 250 2.0.0 Ok: queued as XXXXXXXX
  ## >> QUIT
  ## << 221 2.0.0 Bye
  send_command("QUIT", 221)
}

##' @export
sendmail <- function(from, to, subject, msg, ...,
                     headers=list(),
                     control=list()) {
  ## Argument checks:
  stopifnot(is.list(headers), is.list(control))
  
  get_value <- function(n, default="") {
    if (n %in% names(control)) {
      return(control[[n]])
    } else if (n %in% names(.SendmailREnv$options)) {
      return(.SendmailREnv$options[[n]])
    } else {
      return(default)      
    }
  }

  headers$From <- from
  headers$To <- to
  headers$Subject <- subject

  transport <- get_value("transport", "smtp")
  verbose <- get_value("verbose", FALSE)
  if (transport == "smtp") {
    server <- get_value("smtpServer", "localhost")
    port <- get_value("smtpPort", 25)
    
    .smtp_submit_mail(server, port, headers, msg, verbose)
  } else if (transport == "debug") {
    .write_mail(headers, msg, stdout())
  }
}
