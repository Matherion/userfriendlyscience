\name{formatCI}
\alias{formatCI}
\title{
Pretty formatting of confidence intervals
}
\description{
Pretty much does what the title says.
}
\usage{
formatCI(ci, sep = "; ",
         prefix = "[", suffix = "]",
         digits = 2, noZero = FALSE)
}
\arguments{
  \item{ci}{
  A confidence interval (a vector of 2 elements; longer vectors work, but I guess that wouldn't make sense).
}
  \item{sep}{
  The separator of the values, usually "; " or ", ".
}
  \item{prefix}{
  The prefix, usually a type of opening parenthesis/bracket.
}
  \item{suffix}{
  The suffix, usually a type of closing parenthesis/bracket.
}
  \item{digits}{
  The number of digits to which to round the values.
}
  \item{noZero}{
  Whether to strip the leading zero (before the decimal point), as is typically done when following APA style and displaying correlations, \emph{p} values, and other numbers that cannot reach 1 or more.
}
}
\value{
A character vector of one element.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{noZero}}, \code{\link{formatR}}, \code{\link{formatPvalue}}
}
\examples{
### With leading zero ...
formatCI(c(0.55, 0.021));

### ... and without
formatCI(c(0.55, 0.021), noZero=TRUE);
}
\keyword{ utilities }
