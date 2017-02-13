\name{diamondPlot}
\alias{diamondPlot}
\title{
Basic diamond plot construction function
}
\description{
This function constructs a diamond plot using \code{\link{ggDiamondLayer}}. It's normally not necessary to call this function directly: instead, use \code{\link{meansDiamondPlot}}, \code{\link{meanSDtoDiamondPlot}}, and \code{\link{factorLoadingDiamondCIplot}}.
}
\usage{
diamondPlot(data, ciCols = 1:3,
            colorCol = NULL, otherAxisCol = NULL,
            yValues = NULL, yLabels = NULL,
            ylab = NULL, autoSize = NULL,
            fixedSize = 0.15,
            xlab = "Effect Size Estimate",
            theme = theme_bw(),
            color = "black",
            returnLayerOnly = FALSE,
            ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  A dataframe (or matrix) containing lower bounds, centers (e.g. means), and upper bounds of intervals (e.g. confidence intervals).
}
  \item{ciCols}{
  The columns in the dataframe with the lower bounds, centers (e.g. means), and
  upper bounds (in that order).
}
  \item{colorCol}{
  The column in the dataframe containing the colors for each
  diamond, or a vector with colors (with as many elements as
  the dataframe has rows).
}
  \item{otherAxisCol}{
  The column in the dataframe containing the values that
  determine where on the Y axis the diamond should be
  placed. If this is not available in the dataframe,
  specify it manually using \code{yValues}.
}
  \item{yValues}{
  The values that determine where on the Y axis the diamond should be
  placed (can also be a column in the dataframe; in that case,
  use \code{otherAxisCol}.
}
  \item{yLabels}{
  The labels to use for for each diamond (placed on the Y axis).
}
  \item{xlab, ylab}{
  The labels of the X and Y axes.
}
  \item{autoSize}{
  Whether to make the height of each diamond conditional upon
  its length (the width of the confidence interval).
}
  \item{fixedSize}{
  If not using relative heights, \code{fixedSize} determines
  the height to use.
}
  \item{theme}{
  The theme to use.
}
  \item{color}{
  Color to use if colors are specified for each diamond.
}
  \item{returnLayerOnly}{
  Set this to TRUE to only return the \code{\link{ggplot}} layer of the diamondplot, which can be useful to include it in other plots.
}
  \item{\dots}{
  Additional arguments will be passed to \code{\link{ggDiamondLayer}}.
}
}
\value{
A \code{\link{ggplot}} plot with a \code{\link{ggDiamondLayer}} is returned.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{meansDiamondPlot}}, \code{\link{meanSDtoDiamondPlot}}, \code{\link{factorLoadingDiamondCIplot}}, \code{\link{ggDiamondLayer}}
}
\examples{
tmpDf <- data.frame(lo = c(1, 2, 3),
                    mean = c(1.5, 3, 5),
                    hi = c(2, 4, 10),
                    color = c('green', 'red', 'blue'));
                    
### A simple diamond plot
diamondPlot(tmpDf);

### A diamond plot using the specified colours
diamondPlot(tmpDf, colorCol = 4);

### A diamond plot using automatically generated colours
### using a gradient
diamondPlot(tmpDf, generateColors=c('green', 'red'));

### A diamond plot using automatically generated colours
### using a gradient, specifying the minimum and maximum
### possible values that can be attained
diamondPlot(tmpDf, generateColors=c('green', 'red'),
            fullColorRange=c(1, 10));

}
\keyword{ hplot }
