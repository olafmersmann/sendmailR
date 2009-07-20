##
## sendmailR.r - send email from within R
##
## Author:
##  Olaf Mersmann (OME) <olafm@datensplitter.net>
##

smtpSubmitMail <- function(server, port, from, to, headers, msg, verbose=FALSE) {
  waitFor <- function(lcode) {
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

  sendCmd <- function(cmd, code) {
    if (verbose)
      message(">> ", cmd)
    writeLines(cmd, sock, sep="\r\n")
    waitFor(code)
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
  waitFor(220)
  ## >> HELO localhost
  ## << 250 mail.statistik.uni-dortmund.de
  sendCmd(paste("HELO ", nodename), 250)
  ## >> MAIL FROM: <foo@bah.com>
  ## << 250 2.1.0 Ok
  sendCmd(paste("MAIL FROM: ", from), 250)
  ## >> RCPT TO: <bah@baz.org>
  ## << 250 2.1.5 Ok
  sendCmd(paste("RCPT TO: ", to), 250)
  ## >> DATA
  ## << 354 blah fu
  sendCmd("DATA", 354)
  ## >> <actual message + headers + .>
  if (verbose)
    message(">> <message data>")
  headers$From <- from
  headers$To <- to  
  writeLines(paste(names(headers), unlist(headers), sep=": "), sock, sep="\r\n")
  writeLines("", sock, sep="\r\n")
  writeLines(msg, sock, sep="\r\n")
  writeLines(".", sock, sep="\r\n")
  if (verbose) {
    writeLines(paste(names(headers), unlist(headers), sep=": "))
    writeLines("")
    writeLines(msg)
    message(">> EOM marker")
  }
  waitFor(250)
  ## << 250 2.0.0 Ok: queued as XXXXXXXX
  ## >> QUIT
  ## << 221 2.0.0 Bye
  sendCmd("QUIT", 221)
}

sendmail <- function(from, to, subject, msg, ...,
                     headers=list(),
                     control=list()) {
  ## Argument checks:
  stopifnot(is.list(headers))
  stopifnot(is.list(control))
  
  getValue <- function(n, default="") {
    if (n %in% names(control)) {
      return(control[[n]])
    } else if (n %in% names(.SendmailREnv$options)) {
      return(.SendmailREnv$options[[n]])
    } else {
      return(default)      
    }
  }

  headers$Subject <- subject

  server <- getValue("smtpServer", "localhost")
  port <- getValue("smtpPort", 25)
  verbose <- getValue("verbose", FALSE)
  
  smtpSubmitMail(server, port, from, to, headers, msg, verbose)
}

## Option managment shamelessly taken from the lattice package.
.SendmailREnv <- new.env(parent=emptyenv())
.SendmailREnv$options <- list()

updateList <- function (x, val) {
  if (is.null(x)) 
    x <- list()
  modifyList(x, val)
}

sendmailOptions <- function(...) {
  new <- list(...)
  if (is.null(names(new)) && length(new) == 1 && is.list(new[[1]])) 
    new <- new[[1]]
  old <- .SendmailREnv$options
  if (length(new) == 0) 
    return(old)
  nm <- names(new)
  if (is.null(nm)) 
    return(old[unlist(new)])
  isNamed <- nm != ""
  if (any(!isNamed)) 
    nm[!isNamed] <- unlist(new[!isNamed])
  retVal <- old[nm]
  names(retVal) <- nm
  nm <- nm[isNamed]
  .SendmailREnv$options <- updateList(old, new[nm])
  invisible(retVal)
}
