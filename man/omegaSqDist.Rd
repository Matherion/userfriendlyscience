\name{omegaSqDist}
\alias{domegaSq}
\alias{pomegaSq}
\alias{qomegaSq}
\alias{romegaSq}
\title{
The distribution of Omega Squared
}
\description{
These functions use some conversion to and from the \emph{F} distribution to provide the Omega Squared distribution.
}
\usage{
domegaSq(x, df1, df2, populationOmegaSq = 0)
pomegaSq(q, df1, df2, populationOmegaSq = 0, lower.tail = TRUE)
qomegaSq(p, df1, df2, populationOmegaSq = 0, lower.tail = TRUE)
romegaSq(n, df1, df2, populationOmegaSq = 0)

}
\arguments{
  \item{x, q}{
  Vector of quantiles, or, in other words, the value(s) of Omega Squared.
}
  \item{p}{
  Vector of probabilites (\emph{p}-values).
}
  \item{df1, df2}{
  Degrees of freedom for the numerator and the denominator, respectively.
}
  \item{n}{
  Desired number of Omega Squared values.
}
  \item{populationOmegaSq}{
  The value of Omega Squared in the population; this determines the center of the Omega Squared distribution. This has not been implemented yet in this version of \code{userfriendlyscience}. If anybody has the inverse of \code{\link{convert.ncf.to.omegasq}} for me, I'll happily integrate this.
}
  \item{lower.tail}{
  logical; if TRUE (default), probabilities are the likelihood of finding an Omega Squared smaller than the specified value; otherwise, the likelihood of finding an Omega Squared larger than the specified value.
  }
}
\details{
  The functions use \code{\link{convert.omegasq.to.f}} and \code{\link{convert.f.to.omegasq}} to provide the Omega Squared distribution.
}
\value{
\code{domegaSq} gives the density, \code{pomegaSq} gives the distribution function, \code{qomegaSq} gives the quantile function, and \code{romegaSq} generates random deviates.

}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{convert.omegasq.to.f}}, \code{\link{convert.f.to.omegasq}}, \code{\link{df}}, \code{\link{pf}}, \code{\link{qf}}, \code{\link{rf}}
}
\examples{
### Generate 10 random Omega Squared values
romegaSq(10, 66, 3);

### Probability of findings an Omega Squared
### value smaller than .06 if it's 0 in the population
pomegaSq(.06, 66, 3);

}
\keyword{ univar }
