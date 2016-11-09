\name{factorLoadingDiamondCIplot}
\alias{factorLoadingDiamondCIplot}
\title{
Two-dimensional visualisation of factor analyses
}
\description{
This function uses the \code{\link{diamondPlot}} to visualise the results of a factor analyses. Because the factor loadings computed in factor analysis are point estimates, they may vary from sample to sample. The factor loadings for any given sample are usually not relevant; samples are but means to study populations, and so, researchers are usually interested in population values for the factor loadings. However, tables with lots of loadings can quickly become confusing and intimidating. This function aims to facilitate working with and interpreting factor analysis based on confidence intervals by visualising the factor loadings and their confidence intervals.
}
\usage{
factorLoadingDiamondCIplot(fa,
                           xlab="Factor Loading",
                           colors =
                             brewer.pal(max(3,
                                            fa$factors),
                                        "Set1"),
                           ...)
}
\arguments{
  \item{fa}{
  The object produced by the \code{\link{fa}} function from the \code{\link{psych}} package. It is important that the \code{n.iter} argument of \code{\link{fa}} was set to a realistic number, because otherwise, no confidence intervals will be available.
}
  \item{xlab}{
  The label for the x axis.
}
  \item{colors}{
  The colors used for the factors. The default uses the 'Set1' palette from colorbrewer using the \code{\link{RColorBrewer}} package. A vector can also be supplied; the colors must be valid arguments to \code{\link{colorRamp}} (and therefore, to \code{\link{col2rgb}}).
}
  \item{\dots}{
  Additional arguments will be passed to \code{\link{ggDiamondLayer}}. This can be used to set, for example, the transparency (alpha value) of the diamonds to a lower value using e.g. \code{alpha=.5}.
}
}
\value{
A \code{\link{ggplot}} plot with several \code{\link{ggDiamondLayer}}s is returned.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{fa}}, \code{\link{meansDiamondPlot}}, \code{\link{meanSDtoDiamondPlot}}, \code{\link{diamondPlot}}, \code{\link{ggDiamondLayer}}
}
\examples{
\dontrun{
### Not run because it takes too long and may generate
### warnings because of the bootstrapping of the confidence
### intervals
factorLoadingDiamondCIplot(fa(Thurstone.33, 2,
                              n.iter=100, n.obs=100));

### And using a lower alpha value for the diamonds to
### make them more transparent
factorLoadingDiamondCIplot(fa(Thurstone.33, 2,
                              n.iter=100, n.obs=100),
                           alpha = .5);
}
}
\keyword{ hplot }
