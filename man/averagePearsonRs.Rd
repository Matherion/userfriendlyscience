\name{averagePearsonRs}
\alias{averagePearsonRs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
averagePearsonRs
}
\description{
Takes pairs of Pearson r's (correlation coefficients) and the
accompanying n's (sample sizes) and returns their average.
}
\usage{
averagePearsonRs(rs, ns, FishersZ = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rs}{
  The correlation coefficients.
}
  \item{ns}{
  The sample sizes.
}
  \item{FishersZ}{
  Whether to compute the average through Fisher's z (only method implemented
  as of the writing of this document).
}
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{averageFishersZs}}, \code{\link{convert.r.to.fisherz}}
}
\examples{
averagePearsonRs(c(.3, .4, .6), c(70, 80, 50));
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univariate }
