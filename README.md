# sendmailR

<!-- badges: start -->
[![OSS Lifecycle](https://img.shields.io/osslifecycle/olafmersmann/sendmailR)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN status](https://www.r-pkg.org/badges/version/sendmailR)](https://CRAN.R-project.org/package=sendmailR) 
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/sendmailR)](https://CRAN.R-project.org/package=sendmailR) 
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/sendmailR)](https://CRAN.R-project.org/package=sendmailR)
<!-- badges: end -->

Package contains a simple SMTP client with minimal dependencies which provides a portable solution for sending email, including file attachments and inline html reports, from within [R](https://www.r-project.org/). SMTP Authentication and SSL/STARTTLS is implemented using curl.

## Usage

``` r
from <- sprintf("<sendmailR@@\%s>", Sys.info()[4]) 
to <- "<olafm@@datensplitter.net>" 
subject <- "Hello from R" 
body <- list("It works!", mime_part(iris)) 
sendmail(from, to, subject, body, 
  control=list(smtpServer="ASPMX.L.GOOGLE.COM")) 

# With authentication and SSL
sendmail(from="from@example.org", 
  to=c("to1@example.org","to2@example.org"), 
  subject="SMTP auth test", 
  msg=mime_part("This message was send using sendmailR and curl."), 
  engine = "curl", 
  engineopts = list(username = "foo", password = "bar"), 
  control=list(smtpServer="smtp://smtp.gmail.com:587", verbose = TRUE) 
)
```

## Install from github

To install the bleeding edge version from GitHub using [`devtools`](https://github.com/r-lib/devtools):

```splus
library("devtools")
install_github("olafmersmann/sendmailR")
```

Or using [`pak`](https://github.com/r-lib/pak):

```splus
library("pak")
pkg_install("olafmersmann/sendmailR")
```
