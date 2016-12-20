\name{pwr.cohensdCI}
\alias{pwr.cohensdCI}
\title{
Determine required sample size for a given confidence interval width for Cohen's d
}
\description{
This function computes how many participants you need if you want to achieve a confidence interval of a given width. This is useful when you do a study and you are interested in how strongly twovariables are associated.
}
\usage{
pwr.cohensdCI(d, w = 0.1, conf.level = 0.95, extensive = FALSE, silent=FALSE)
}
\arguments{
  \item{d}{
  The expected value(s) of Cohen's d.
}
  \item{w}{
  The half-width of the desired confidence interval (the margin of error).
}
  \item{conf.level}{
  The confidence level of the desired confidence interval.
}
  \item{extensive}{
  Whether you want extensive results or just the sample size(s).
}
  \item{silent}{
  Whether you want warnings or not.
}
}
\value{
The sample size(s) you need (or, if \code{extensive} is \code{TRUE}, also the requested and resulting confidence interval bounds).
}
\references{
Peters, G. J. Y. & Crutzen, R. (forthcoming) An easy and foolproof method for establishing how effective an intervention or behavior change method is: required sample size for accurate parameter estimation in health psychology.

Maxwell, S. E., Kelley, K., & Rausch, J. R. (2008). Sample size planning for statistical power and accuracy in parameter estimation. Annual Review of Psychology, 59, 537-63. https://doi.org/10.1146/annurev.psych.59.103006.093735

Cumming, G. (2013). The New Statistics: Why and How. Psychological Science, (November). https://doi.org/10.1177/0956797613504966

}
\author{
Gjalt-Jorn Peters
}
\seealso{
\code{\link{cohensdCI}}
}
\examples{

### If you want to estimate an expected effect size of
### 0.5 (tentatively qualified as a 'moderate' association),
### and you want to estimate with a maximum confidence interval
### width of .1 (e.g. from .45 to .55), you'll need a lot of people:
pwr.cohensdCI(.5, w=.05);

### If you expect a smaller effect and are content with a less
### accurate estimation, you'll need way less (but still a lot):
pwr.cohensdCI(.2, w=.1);

### If you want more extensive feedback:
pwr.cohensdCI(.2, w=.1, extensive=TRUE);

### As Cohen's d becomes larger, the confidence interval widens and
### therefore, you need more people:
pwr.cohensdCI(c(.2, .5, .8), w=.1);

}
\keyword{ htest }
