\name{dCohensd}
\alias{dCohensd}
\alias{pCohensd}
\alias{qCohensd}
\alias{rCohensd}
\alias{dd}
\alias{pd}
\alias{qd}
\alias{rd}
\alias{pdExtreme}
\alias{pdMild}
\alias{pdInterval}
\alias{cohensdCI}
\title{
The distribution of Cohen's \emph{d}
}
\description{
These functions use some conversion to and from the \emph{t} distribution to provide the Cohen's \emph{d} distribution. There are four versions that act similar to the standard distribution functions (e.g. the \code{d.}, \code{p.}, \code{q.}, and \code{r.} functions), and three convenience functions.
}
\usage{
dd(x, df, populationD = 0)
pd(q, df, populationD = 0, lower.tail = TRUE)
qd(p, df, populationD = 0, lower.tail = TRUE)
rd(n, df, populationD = 0)

dCohensd(x, df, populationD = 0)
pCohensd(q, df, populationD = 0, lower.tail = TRUE)
qCohensd(p, df, populationD = 0, lower.tail = TRUE)
rCohensd(n, df, populationD = 0)

pdExtreme(d, n, populationD=0)
pdMild(d, n, populationD=0)
pdInterval(ds, n, populationD=0)

cohensdCI(d, n, conf.level = .95, plot=FALSE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, q, d}{
  Vector of quantiles, or, in other words, the value(s) of Cohen's \emph{d}.
}
  \item{ds}{
  A vector with two Cohen's \emph{d} values.
}
  \item{p}{
  Vector of probabilites (\emph{p}-values).
}
  \item{df}{
  Degrees of freedom.
}
  \item{n}{
  Desired number of Cohen's \emph{d} values for \code{rCohensd} and \code{rd}, and the number of participants/datapoints for \code{pdExtreme}, \code{pdMild}, and \code{pdInterval}.
}
  \item{populationD}{
  The value of Cohen's \emph{d} in the population; this determines the center of the Cohen's \emph{d} distribution. I suppose this is the noncentrality parameter.
}
  \item{lower.tail}{
  logical; if TRUE (default), probabilities are the likelihood of finding a Cohen's \emph{d} smaller than the specified value; otherwise, the likelihood of finding a Cohen's \emph{d} larger than the specified value.
}
  \item{conf.level}{
  The level of confidence of the confidence interval.
}
  \item{plot}{
  Whether to show a plot of the sampling distribution of Cohen's \emph{d} and the confidence interval. This can only be used if specifying one value for \code{d}, \code{n}, and \code{conf.level}.
}
}
\details{
  The functions use \code{\link{convert.d.to.t}} and \code{\link{convert.t.to.d}} to provide the Cohen's \emph{d} distribution.
}
\value{
\code{dCohensd} (or \code{dd}) gives the density, \code{pCohensd} (or \code{pd}) gives the distribution function, \code{qCohensd} (or \code{qd}) gives the quantile function, and \code{rCohensd} (or \code{rd}) generates random deviates.

\code{pdExtreme} returns the probability (or probabilities) of finding a Cohen's \emph{d} equal to or more extreme than the specified value(s).

\code{pdMild} returns the probability (or probabilities) of finding a Cohen's \emph{d} equal to or \emph{less} extreme than the specified value(s).

\code{pdInterval} returns the probability of finding a Cohen's \emph{d} that lies in between the two specified values of Cohen's \emph{d}.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{convert.d.to.t}}, \code{\link{convert.t.to.d}}, \code{\link{dt}}, \code{\link{pt}}, \code{\link{qt}}, \code{\link{rt}}
}
\examples{
### Generate 10 random Cohen's d values
rCohensd(10, 20, populationD = .5);

### Probability of findings a Cohen's d smaller than
### .5 if it's 0 in the population (i.e. under the
### null hypothesis)
pCohensd(.5, 64);

### Probability of findings a Cohen's d larger than
### .5 if it's 0 in the population (i.e. under the
### null hypothesis)
1 - pCohensd(.5, 64);

### Probability of findings a Cohen's d more extreme
### than .5 if it's 0 in the population (i.e. under
### the null hypothesis)
pdExtreme(.5, 64);

### Probability of findings a Cohen's d more extreme
### than .5 if it's 0.2 in the population.
pdExtreme(.5, 64, populationD = .2);

}
\keyword{ univar }
