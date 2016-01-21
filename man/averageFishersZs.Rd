\name{averageFishersZs}
\alias{averageFishersZs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
averageFishersZs
}
\description{
Takes pairs of Fisher's z's and the accompanying n's (sample sizes) and
returns their average.
}
\usage{
averageFishersZs(zs, ns)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{zs}{
The values of Fisher's z.
}
  \item{ns}{
The sample sizes (ns).
}
}
\value{
The average of the Fisher's z values.
}
\seealso{
\code{\link{averagePearsonRs}}
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
averageFishersZs(c(1.1, 5.4), c(10, 30));
}