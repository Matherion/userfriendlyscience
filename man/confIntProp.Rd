\name{confIntProp}
\alias{confIntProp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Confidence intervals for proportions, vectorized over all arguments
}
\description{

}
\usage{
confIntProp(x, n, conf.level = 0.95)
}
\arguments{
  \item{x}{
  The number of 'successes', i.e. the number of events, observations, or cases that one is interested in.
}
  \item{n}{
  The total number of cases or observatons.
}
  \item{conf.level}{
  The confidence level.
}
}
\details{
This function is the adapted source code of \code{\link{binom.test}}. Ir uses \code{\link{pbeta}}, with some lines of code taken from the \code{\link{binom.test}} source. Specifically, the count for the low category is specified as first 'shape argument' to \code{\link{pbeta}}, and the total count (either the sum of the count for the low category and the count for the high category, or the total number of cases if \code{compareHiToLo} is \code{FALSE}) minus the count for the low category as the second 'shape argument'.


}
\value{
The confidence interval bounds in a twodimensional matrix, with the first column containing the lower bound and the second column containing the upper bound.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{binom.test}} and \code{\link{ggPorportionPlot}, the function for which this was written.}
}
\examples{
  ### Simple case
  confIntProp(84, 200);
  
  ### Using vectors
  confIntProp(c(2,3), c(10, 20), conf.level=c(.90, .95, .99));
}
\keyword{ univar }
\keyword{ htest }
