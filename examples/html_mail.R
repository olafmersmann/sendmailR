library("sendmailR")

from <- sprintf("<sendmailR@\\%s>", Sys.info()[4])
to <- "<olafm@statistik.tu-dortmund.de>"
subject <- "Hello from R"
f <- file("foo.html", open="w")
writeLines("<html><head>
<title>HTML mail</title>
</head>
<body>
<b>It</b> seems to <em>work</em>!
</body>
</html>", con=f)
close(f)

body <- list(mime_part("msg.html", type="text/html", disposition="inline"))
sendmail(from, to, subject, body,
         control=list(smtpServer="mail.statistik.tu-dortmund.de"))
