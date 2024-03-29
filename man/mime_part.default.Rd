% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mime_part.R
\name{mime_part.default}
\alias{mime_part.default}
\title{Default MIME part method}
\usage{
\method{mime_part}{default}(x, name, ...)
}
\arguments{
\item{x}{R object}

\item{name}{Filename used for attachment (sans the .R extension)}

\item{...}{Ignored.}
}
\value{
An S3 \code{mime_part} object.
}
\description{
Creates a string representation of the object \code{x} using
\code{dput}. This representation is then turned into a file
attachment.
}
