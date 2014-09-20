library("sendmailR")
from <- sprintf("<sendmailR@%s>", Sys.info()[4])
to <- "olafm@statistik.tu-dortmund.de"
subject <- "Hello from R"
ody <- list("It works!", mime_part(iris))

sendmail(from, to, subject, body,
         control=list(smtpServer="mail.statistik.tu-dortmund.de"))
