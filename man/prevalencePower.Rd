\name{prevalencePower}
\alias{prevalencePower}
\title{
Power analysis for establishing a prevalence
}
\description{
This function can be used to establish how many participants are required to establish a prevalence rate with a given margin of error.
}
\usage{
prevalencePower(expectedPrevalence,
                marginOfError = 0.05,
                conf.level = 0.95)
}
\arguments{
  \item{expectedPrevalence}{
  The expected prevalence.
}
  \item{marginOfError}{
  The desired precision.
}
  \item{conf.level}{
  The confidence of the confidence interval.
}
}
\details{
Note that when uncertain as to the expected prevalence, it's better to assume a prevalence closer to 50\%. Prevalences closer to 0\% or 100\% are easier to detect and therefore have more power.
}
\value{
The required number of participants.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{convert.percentage.to.se}}
}
\examples{
### Required participants for detecting a prevalence of 10\%
### with a 95\% confidence interval of 10\% wide:
prevalencePower(.1);

### Required participants for detecting a prevalence of 10\%
### with a 95\% confidence interval of 4\% wide:
prevalencePower(.1, .02);

### Required participants for detecting a prevalence of 60\%
### with a 95\% confidence interval of 10\% wide:
prevalencePower(.6);
}
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
