\name{RsqDist}
\alias{dRsq}
\alias{pRsq}
\alias{qRsq}
\alias{rRsq}
\title{
The distribution of R squared (as obtained in a regression analysis)
}
\description{
These functions use the beta distribution to provide the R Squared distribution.
}
\usage{
dRsq(x, nPredictors, sampleSize, populationRsq = 0)
pRsq(q, nPredictors, sampleSize, populationRsq = 0, lower.tail = TRUE)
qRsq(p, nPredictors, sampleSize, populationRsq = 0, lower.tail = TRUE)
rRsq(n, nPredictors, sampleSize, populationRsq = 0)

}
\arguments{
  \item{x, q}{
  Vector of quantiles, or, in other words, the value(s) of R Squared.
}
  \item{p}{
  Vector of probabilites (\emph{p}-values).
}
  \item{nPredictors}{
  The number of predictors.
}
  \item{sampleSize}{
  The sample size.
}
  \item{n}{
  The number of R Squared values to generate.
}
  \item{populationRsq}{
  The value of R Squared in the population; this determines the center of the R Squared distribution. This has not been implemented yet in this version of \code{userfriendlyscience}. If anybody knows how to do this and lets me know, I'll happily integrate this of course.
}
  \item{lower.tail}{
  logical; if TRUE (default), probabilities are the likelihood of finding an R Squared smaller than the specified value; otherwise, the likelihood of finding an R Squared larger than the specified value.
  }
}
\details{
  The functions use \code{\link{convert.omegasq.to.f}} and \code{\link{convert.f.to.omegasq}} to provide the Omega Squared distribution.
}
\value{
\code{dRsq} gives the density, \code{pRsq} gives the distribution function, \code{qRsq} gives the quantile function, and \code{rRsq} generates random deviates.

}
\note{
These functions are based on the Stack Exchange (Cross Validated) post at \url{http://stats.stackexchange.com/questions/130069/what-is-the-distribution-of-r2-in-linear-regression-under-the-null-hypothesis}. Thus, the credits go to Alecos Papadopoulos, who provided the answer that was used to write these functions.
}
\author{
Gjalt-Jorn Peters (based on a CrossValidated answer by Alecos Papadopoulos)

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{dbeta}}, \code{\link{pbeta}}, \code{\link{qbeta}}, \code{\link{rbeta}}
}
\examples{
### Generate 10 random R Squared values
### with 2 predictors and 100 participants
rRsq(10, 2, 100);

### Probability of finding an R Squared of
### .15 with 4 predictors and 100 participants
pRsq(.15, 4, 100, lower.tail = FALSE);

### Probability of finding an R Squared of
### .15 with 15 predictors and 100 participants
pRsq(.15, 15, 100, lower.tail=FALSE);

}
\keyword{ univar }
