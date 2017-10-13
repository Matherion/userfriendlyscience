\name{findShortestInterval}
\alias{findShortestInterval}
\title{
Find the shortest interval
}
\description{
This function takes a numeric vector, sorts it, and then finds the shortest interval and returns its length.
}
\usage{
findShortestInterval(x)
}
\arguments{
  \item{x}{
  The numeric vector.
}
}
\value{
The length of the shortest interval.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
findShortestInterval(c(1, 2, 4, 7, 20, 10, 15));
}
\keyword{ utilities }
