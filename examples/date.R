library("sendmailR")

rfc2822_date <- function(time=Sys.time()) {
  lc <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", lc))
  Sys.setlocale("LC_TIME", "C")
  strftime(time, format="%a, %d %b %Y %H:%M:%S -0000",
           tz="UTC", use.tz=TRUE)
}

from <- "sendmailR@p-value.net"
to <- "olafm@statistik.tu-dortmund.de"
subject <- "Hello from R"
body <- list("It works!", mime_part(iris))

headers <- list(Date=rfc2822_date())

sendmail(from, to, subject, body,
         control=list(smtpServer="mail.statistik.tu-dortmund.de"),
         headers=headers)
