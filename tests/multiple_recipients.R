library("sendmailR")

sendmail(from="from@example.org",
         to="to1@example.org",
         subject="foo",
         msg="bar",
         control=list(transport="debug"))

sendmail(from="from@example.org",
         to=c("to1@example.org", "to2@example.org"),
         subject="foo",
         msg="bar",
         control=list(transport="debug"))

sendmail(from="from@example.org",
         to=c("to1@example.org", "to2@example.org"),
         cc="cc1@example.org",
         subject="foo",
         msg="bar",
         control=list(transport="debug"))

sendmail(from="from@example.org",
         to=c("to1@example.org", "to2@example.org"),
         cc="cc1@example.org",
         subject="foo",
         msg="bar",
         control=list(transport="debug"))

sendmail(from="from@example.org",
         to="to1@example.org",
         cc=c("cc1@example.org", "cc2@example.org"),
         subject="foo",
         msg="bar",
         control=list(transport="debug"))

sendmail(from="from@example.org",
         to="to1@example.org",
         bcc="bcc1@example.org",
         subject="foo",
         msg="bar",
         control=list(transport="debug"))

sendmail(from="from@example.org",
         to="to1@example.org",
         bcc=c("bcc1@example.org", "bcc2@example.org"),
         subject="foo",
         msg="bar",
         control=list(transport="debug"))

## Subject encoding
sendmail(from="from@example.org",
         to="to1@example.org",
         subject="K\u00f6ln",
         msg="bar",
         control=list(transport="debug"))

## Character MIME part encoding
sendmail(from="from@example.org",
         to="to1@example.org",
         subject="UTF-8 test",
         msg=mime_part(c("test", "tεst", "täst", "tëst", "的", "؈")),
         control=list(transport="debug"))

sendmail(from="from@example.org",
         to="to1@example.org",
         subject="UTF-8 test",
         msg=mime_part(c("test", "tεst", "täst", "tëst", "的", "؈"), flowed = TRUE),
         control=list(transport="debug"))

sendmail(from="from@example.org",
         to="to1@example.org",
         subject="latin1 test",
         msg=mime_part(iconv("täst", to = "latin1")),
         control=list(transport="debug"))

## Inline HMTL MIME part
sendmail(from="from@example.org",
         to="to1@example.org",
         subject="Simple inline HTML",
         msg=mime_part_html("a<br>b"),
         control=list(transport="debug"))

sendmail(from="from@example.org",
         to="to1@example.org",
         subject="Inline HTML from file",
         msg=mime_part_html(system.file("afm/MustRead.html", package = "grDevices")),
         control=list(transport="debug"))
