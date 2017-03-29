\name{escapeRegex}
\alias{escapeRegex}
\alias{escapeBS}
\title{ Escapes any characters that would have special meaning in a reqular expression. }
\description{
  Escapes any characters that would have special meaning in a reqular expression.
}
\usage{
escapeRegex(string)
}
\arguments{
  \item{string}{ string being operated on. }
}
\details{
  \code{escapeRegex} will escape any characters that would have
  special meaning in a reqular expression. For any string
  \code{grep(regexpEscape(string), string)} will always be true.
}
\value{
  The value of the string with any characters that would have
  special meaning in a reqular expression escaped.
}
\note{
  Note that this function was copied literally from the \code{Hmisc} package (to prevent importing the entire package for one line of code).
}
\author{
  Charles Dupont\cr
  Department of Biostatistics\cr
  Vanderbilt University

  Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{ \code{\link[base]{grep}}, \code{Hmisc}, \url{http://biostat.mc.vanderbilt.edu/wiki/Main/Hmisc}, \url{https://github.com/harrelfe/Hmisc} }
\examples{
string <- "this\\\\(system) {is} [full]."
escapeRegex(string)
\dontshow{
if(!any(grep(escapeRegex(string), string))) {
  stop("function escapeRegex failed test")
}
}
}
\keyword{ manip }% at least one, from doc/KEYWORDS
\keyword{ character }% __ONLY ONE__ keyword per line
\keyword{ programming }
