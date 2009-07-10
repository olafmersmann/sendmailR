\name{sendmailOptions}
\Rdversion{1.1}
\alias{sendmailOptions}
\title{Set package specific options.}
\description{
  Specify global sendmail options so that subsequent calls to
  \code{sendmail()} do not have to set them in the \code{control}
  argument.  
}
\usage{
sendmailOptions(...)
}
\arguments{
  \item{\dots}{any options can be defined, using \code{name=value} or by
    passing a list of such tagged values.  However, only the ones
    below are used in base sendmailR.}
}
\details{
  List of options: ...
}
\value{
  For \code{sendmailOptions()}, a list of all set options sorted by name.  For
  \code{sendmailOptions(name)}, a list of length one containing the set value, or
  'NULL' if it is unset.  For uses setting one or more options, a
  list with the previous values of the options changed (returned
  invisibly).
}
\author{Olaf Mersmann \email{olafm@datensplitter.net}}
\keyword{utilities}