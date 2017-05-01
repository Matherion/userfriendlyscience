\name{areColors}
\alias{areColors}
\title{
Check whether elements of a vector are valid colors
}
\description{
This function by Josh O'Brien checks whether elements of a vector are valid colors. It has been copied from a Stack Exchange answer (see \url{http://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation}).
}
\usage{
areColors(x)
}
\arguments{
  \item{x}{
  The vector.
}
}
\value{
A logical vector.
}
\author{
Josh O'Brien

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
areColors(c(NA, "black", "blackk", "1", "#00", "#000000"));
}
\keyword{ utilities }
