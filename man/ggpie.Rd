\name{ggpie}
\alias{ggpie}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ggpie
}
\description{
This function, very strongly based on the Mathematical Coffee post at http://mathematicalcoffee.blogspot.nl/2014/06/ggpie-pie-graphs-in-ggplot2.html,
creates a pie chart (note that these are generally quite strongly advised against).
}
\usage{
ggpie(vector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vector}{
  The vector (best to pass a factor).
}
}
\value{
A ggplot pie chart.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
ggpie(mtcars$cyl);
}
\keyword{ univariate }
