\name{readBenc}
\alias{readBenc}
\title{
  Read content encoded in Bencoding
}
\description{
  \code{readBenc} reads content in Bencoding into R objects.
}
\usage{
readBenc(what)
}
\arguments{
  \item{what}{character string of file name(s), binary connection or a
    raw vector}
}
\details{
  Bencoding defines integers, strings, lists and dictionaries which are
  stored as integer (or real) scalars, string scalars, pairlists and named
  parilists respectively.
}
\value{
  Decoded content. If \code{what} is a character vector of more than one
  element then the result is a list equivalent to
  \code{lapply(what, readBenc)}.
}
%\references{
%}
\author{
  Simon Urbanek
}
\note{
  In principle Bencoding supports integers of arbitary precision, but
  R only supports signed integers up to 32-bit. All values outsisde
  that range will be stored as (double-precision) reals. This means
  that only signed integers up to 53-bit precision will be represented
  without loss of precision.

  R does not allow strings with embedded NULs, so any string with a
  value less than TAB is returned as a raw vector.
}
\examples{
  ex <- charToRaw("d1:ai100e1:bl3:fooi123456789012345e3:baree")
  readBenc(ex)
}
\keyword{manip}
