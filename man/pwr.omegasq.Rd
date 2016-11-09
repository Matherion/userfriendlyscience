\name{pwr.omegasq}
\alias{pwr.omegasq}
\title{
Power calculations for Omega Squared.
}
\description{
This function uses \code{\link{pwr.anova.test}} from the \code{\link{pwr}} package in combination with \code{\link{convert.cohensf.to.omegasq}} and \code{\link{convert.omegasq.to.cohensf}} to provide power analyses for Omega Squared.
}
\usage{
pwr.omegasq(k = NULL, n = NULL, omegasq = NULL,
            sig.level = 0.05, power = NULL, digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{
  The number of groups.
}
  \item{n}{
  The sample size.
}
  \item{omegasq}{
  The Omega Squared value.
}
  \item{sig.level}{
  The significance level (alpha).
}
  \item{power}{
  The power.
}
  \item{digits}{
  The number of digits desired in the output (4, the default, is quite high; but omega squared value tend to be quite low).
}
}
\details{
This function was written to work similarly to the power functions in the \code{\link{pwr}} package.
}
\value{
An \code{power.htest.usf} object that contains a number of input and output values, most notably:

\item{power}{The (specified or computed) power}
\item{n}{The (specified or computed) sample size in each group}
\item{sig.level}{The (specified or computed) significance level (alpha)}
\item{sig.level}{The (specified or computed) Omega Squared value}
\item{cohensf}{The computed value for the Cohen's \emph{f} effect size measure}

}

\author{
Gjalt-Jorn Peters & Peter Verboon

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}

\seealso{
\code{\link{pwr.anova.test}}, \code{\link{convert.cohensf.to.omegasq}}, \code{\link{convert.omegasq.to.cohensf}}
}
\examples{
pwr.omegasq(omegasq=.06, k=3, power=.8)
}

\keyword{ htest }
