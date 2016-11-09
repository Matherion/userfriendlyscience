\name{regrInfluential}
\alias{regrInfluential}
\title{
Detecting influential cases in regression analyses
}
\description{
This function combines a number of criteria for determining whether a datapoint is an influential case in a regression analysis. It then sum the criteria to compute an index of influentiality. A list of cases with an index of influentiality of 1 or more is then displayed, after which the regression analysis is repeated without those influantial cases. A scattermatrix is also displayed, showing the density curves of each variable, and in the scattermatrix, points that are colored depending on how influential each case is.
}
\usage{
regrInfluential(formula, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
  The formule of the regression analysis.
}
  \item{data}{
  The data to use for the analysis.
}
}
\value{
A \code{regrInfluential} object, which, if printed, shows the influential cases, the regression analyses repeated without those cases, and the scatter matrix.
}
\author{
Gjalt-Jorn Peters & Marwin Snippe

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
regrInfluential(mpg ~ hp, mtcars);
}
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
