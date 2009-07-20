\name{sendmail}
\Rdversion{1.1}
\alias{sendmail}
\title{Send mail from within R}
\description{
  Simplistic sendmail utility for R. Uses SMTP to submit a message to a
  local SMTP server.
}
\usage{
sendmail(from, to, subject, msg, ..., headers = list(), control = list())
}
\arguments{
  \item{from}{From whom the mail message is (RFC2822 style address).}
  \item{to}{Recipient of the message (valid RFC2822 style address).}
  \item{subject}{Subject line of message.}
  \item{msg}{Body text of message. No MIME encoding or quoting is performed!}
  \item{\dots}{...}
  \item{headers}{Any other headers to include.}
  \item{control}{List of SMTP server settings. Valid values are the
    possible options for \code{\link{sendmailOptions}}.}
}
\author{Olaf Mersmann \email{olafm@datensplitter.net}}
\examples{
\dontrun{
from <- sprintf("<sendmailR@%s>", Sys.info()[4])
to <- "<olafm@datensplitter.net>"
subject <- "Hello from R"
msg <- "It works!"
sendmail(from, to, subject, msg,
         control=list(smtpServer="ASPMX.L.GOOGLE.COM"))
}
}
\keyword{utilities}
