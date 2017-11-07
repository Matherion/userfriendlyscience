\name{validComputations}
\alias{validComputations}
\alias{validMeans}
\alias{validSums}
\title{
Only compute means or sums for cases with enough nonmissings
}
\description{
These functions have been written as equivalents of SPSS' \code{MEAN.x} and \code{SUM.x} functions, which only compute means and sums if enough cases have valid values.
}
\usage{
validMeans(...,
           requiredValidValues = .8,
           returnIfInvalid = NA,
           silent = FALSE)
validSums(...,
          requiredValidValues = .8,
          returnIfInvalid = NA,
          silent = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{
  Either a dataframe or vectors for which to compute the mean or sum.
}
  \item{requiredValidValues}{
  How many values must be valid (i.e. nonmissing) to compute the mean or sum. If a number lower than 1 is provided, it is interpreted as proportion, and the number of variables is computed. For example is \code{requiredValidValues=.8}, 80\% of the variables must have valid values.
}
  \item{returnIfInvalid}{
  Wat to return for cases that don't have enough valid values.
}
  \item{silent}{
  Whether to show the number of cases that have to be valid if \code{requiredValidValues} is a proportion.
}
}
\value{
A numeric vector with the resulting means or sums.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{rowMeans}}, \code{\link{rowSums}}
}
\examples{
validMeans(mtcars$cyl, mtcars$disp);
validSums(mtcars$cyl, mtcars$disp);

### Or specifying a dataframe
validSums(mtcars);
}
\keyword{ manip }
