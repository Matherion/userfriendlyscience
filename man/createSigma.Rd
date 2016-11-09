\name{createSigma}
\alias{createSigma}
\title{
createSigma: convenience function for mvrnorm
}
\description{
This function is made to quickly generate a Sigma matrix of the type required by \code{\link{mvrnorm}}. By specifying the number of variables, the mean correlation, and how much variation there should be in the correlations, it's easy to quickly generate a correlation matrix.
}
\usage{
createSigma(nVar, meanR = 0.3, sdR = 0, diagonal = 1)
}
\arguments{
  \item{nVar}{
  The number of variables in the correlation matrix.
}
  \item{meanR}{
  The average correlation, provided to \code{\link{rnorm}} together with \code{sdR} to generate the correlations.
}
  \item{sdR}{
  The variation in the correlations, provided to \code{\link{rnorm}} together with \code{meanR} to generate the correlations.
}
  \item{diagonal}{
  The value on the diagonal of the returned matrix: will normally be 1.
}
}
\value{
A matrix of nVar x nVar.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{mvrnorm}}, \code{\link{rnorm}}, \code{\link{matrix}}
}
\examples{
createSigma(3, .5, .1);
}
\keyword{ datagen }
