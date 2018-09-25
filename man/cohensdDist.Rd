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
\alias{confIntD}
\alias{pwr.cohensdCI}
\alias{pwr.confIntd}
\title{
The distribution of Cohen's \emph{d}
}
\description{
These functions use some conversion to and from the \emph{t} distribution to provide the Cohen's \emph{d} distribution. There are four versions that act similar to the standard distribution functions (the \code{d.}, \code{p.}, \code{q.}, and \code{r.} functions, and their longer aliases \code{.Cohensd}), three convenience functions (\code{pdExtreme}, \code{pdMild}, and \code{pdInterval}), a function to compute the confidence interval for a Cohen's \emph{d} estimate \code{cohensdCI}, and a function to compute the sample size required to obtain a confidence interval around a Cohen's \emph{d} estimate with a specified accuracy (\code{pwr.cohensdCI} and its alias \code{pwr.confIntd}).
}
\usage{
dd(x, df=NULL, populationD = 0,
   n=NULL, n1=NULL, n2=NULL,
   silent=FALSE)
pd(q, df, populationD = 0, lower.tail = TRUE)
qd(p, df, populationD = 0, lower.tail = TRUE)
rd(n, df, populationD = 0)

dCohensd(x, df=NULL, populationD = 0,
         n=NULL, n1=NULL, n2=NULL,
         silent=FALSE)
pCohensd(q, df, populationD = 0, lower.tail = TRUE)
qCohensd(p, df, populationD = 0, lower.tail = TRUE)
rCohensd(n, df, populationD = 0)

pdExtreme(d, n, populationD=0)
pdMild(d, n, populationD=0)
pdInterval(ds, n, populationD=0)

cohensdCI(d, n, conf.level = .95, plot=FALSE, silent=TRUE)
confIntD(d, n, conf.level = .95, plot=FALSE, silent=TRUE)

pwr.cohensdCI(d, w = 0.1, conf.level = 0.95,
              extensive = FALSE, silent = TRUE)
pwr.confIntd(d, w = 0.1, conf.level = 0.95,
             extensive = FALSE, silent = TRUE)

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
  \item{df, n1, n2}{
  Degrees of freedom or sample sizes for each group (\code{n1} and \code{n2}); also see explanation for \code{n} below.
}
  \item{n}{
  Total \code{n} (\code{df} + 2) for \code{dd} and \code{dCohensd}, desired number of Cohen's \emph{d} values for \code{rCohensd} and \code{rd}, and the number of participants/datapoints for \code{pdExtreme}, \code{pdMild}, \code{pdInterval}, and \code{cohensdCI}.
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
  \item{w}{
  The desired 'half-width' or margin of error of the confidence interval.
}
  \item{extensive}{
  Whether to only return the required sample size, or more extensive results.
}
  \item{silent}{
  Whether to provide \code{FALSE} or suppress (\code{TRUE}) warnings.
  This is useful because function 'qt', which is used under the hood (see
  \code{\link{qt}} for more information), warns that 'full precision may
  not have been achieved' when the density of the distribution is very
  close to zero. This is normally no cause for concern, because
  with sample sizes this big, small deviations have little impact.
}
}
\details{
  The functions use \code{\link{convert.d.to.t}} and \code{\link{convert.t.to.d}} to provide the Cohen's \emph{d} distribution.
  
  More details about \code{cohensdCI} and \code{pwr.cohensdCI} are provided
  in Peters & Crutzen (2017).
}
\value{
\code{dCohensd} (or \code{dd}) gives the density, \code{pCohensd} (or \code{pd}) gives the distribution function, \code{qCohensd} (or \code{qd}) gives the quantile function, and \code{rCohensd} (or \code{rd}) generates random deviates.

\code{pdExtreme} returns the probability (or probabilities) of finding a Cohen's \emph{d} equal to or more extreme than the specified value(s).

\code{pdMild} returns the probability (or probabilities) of finding a Cohen's \emph{d} equal to or \emph{less} extreme than the specified value(s).

\code{pdInterval} returns the probability of finding a Cohen's \emph{d} that lies in between the two specified values of Cohen's \emph{d}.

\code{cohensdCI} provides the confidence interval(s) for a given Cohen's \emph{d} value.

\code{pwr.cohensdCI} provides the sample size required to obtain a confidence interval for Cohen's \emph{d} with a desired width.
}
\references{

Peters, G. J. Y. & Crutzen, R. (2017) Knowing exactly how effective an intervention, treatment, or manipulation is and ensuring that a study replicates: accuracy in parameter estimation as a partial solution to the replication crisis. http://dx.doi.org/

Maxwell, S. E., Kelley, K., & Rausch, J. R. (2008). Sample size planning for statistical power and accuracy in parameter estimation. Annual Review of Psychology, 59, 537-63. https://doi.org/10.1146/annurev.psych.59.103006.093735

Cumming, G. (2013). The New Statistics: Why and How. Psychological Science, (November). https://doi.org/10.1177/0956797613504966

}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{convert.d.to.t}}, \code{\link{convert.t.to.d}}, \code{\link{dt}}, \code{\link{pt}}, \code{\link{qt}}, \code{\link{rt}}
}
\examples{
### Confidence interval for Cohen's d of .5
### from a sample of 200 participants, also
### showing this visually: this clearly shows
### how wildly our Cohen's d value can vary
### from sample to sample.
cohensdCI(.5, n=200, plot=TRUE);

### How many participants would we need if we
### would want a more accurate estimate, say
### with a maximum confidence interval width
### of .2?
pwr.cohensdCI(.5, w=.1);

### Show that 'sampling distribution':
cohensdCI(.5,
          n=pwr.cohensdCI(.5, w=.1),
          plot=TRUE);

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
