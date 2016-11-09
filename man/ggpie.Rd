\name{ggPie}
\alias{ggPie}
\title{
A ggplot pie chart
}
\description{
THis function creates a pie chart. Note that these are generally quite strongly advised against, as people are not good at interpreting relative frequencies on the basis of pie charts.
}
\usage{
ggPie(vector)
}
\arguments{
  \item{vector}{
  The vector (best to pass a factor).
}
}
\value{
A ggplot pie chart.
}
\note{
This function is very strongly based on the Mathematical Coffee post at http://mathematicalcoffee.blogspot.com/2014/06/ggpie-pie-graphs-in-ggplot2.html.
}
\author{
Amy Chan; implemented in this package (and tweaked a bit) by Gjalt-Jorn Peters.

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
ggPie(mtcars$cyl);
}
\keyword{ hplot }
