\name{ggDiamondLayer}
\alias{ggDiamondLayer}
\alias{diamondCoordinates}
\alias{varsToDiamondPlotDf}
\alias{rawDataDiamondLayer}
\title{
Basic ggplot2 diamond plot layer construction functions
}
\description{
These functions are used by \code{\link{diamondPlot}} to construct a diamond plot. It's normally not necessary to call this function directly: instead, use \code{\link{meansDiamondPlot}}, \code{\link{meanSDtoDiamondPlot}}, and \code{\link{factorLoadingDiamondCIplot}}.
}
\usage{
ggDiamondLayer(data, ciCols = 1:3,
               colorCol = NULL,
               generateColors = NULL,
               fullColorRange = NULL,
               color = "black",
               lineColor=NA,
               otherAxisCol = 1:nrow(data),
               autoSize = NULL,
               fixedSize = 0.15,
               ...)
diamondCoordinates(values,
                   otherAxisValue = 1,
                   direction = "horizontal",
                   autoSize = NULL,
                   fixedSize = 0.15)
varsToDiamondPlotDf(dat, items = NULL,
                    labels = NULL, decreasing = NULL, 
                    conf.level = 0.95)
rawDataDiamondLayer(dat, items = NULL,
                    itemOrder = 1:length(items),
                    dataAlpha = 0.1, dataColor = "#444444",
                    jitterWidth = 0.5, jitterHeight = 0.4,
                    size = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data, dat}{
  A dataframe (or matrix) containing lower bounds, centers (e.g. means), and upper bounds of intervals (e.g. confidence intervals) for \code{ggDiamondLayer} or items and raw data for \code{varsToDiamondPlotDf} and \code{rawDataDiamondLayer}.
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
  \item{generateColors}{
  A vector with colors to use to generate a gradient. These colors must be valid arguments to \code{\link{colorRamp}} (and therefore, to \code{\link{col2rgb}}).
}
  \item{fullColorRange}{
  When specifying a gradient using \code{generateColors}, it is usually desirable to specify the minimum and maximum possible value corresponding to the outer anchors of that gradient. For example, when plotting numbers from 0 to 100 using a gradient from 'red' through 'orange' to 'green', none of the means may actually be 0 or 100; the lowest mean may be, for example, 50. If no \code{fullColorRange} is specified, the diamond representing that lowest mean of 50 wil be red, not orange. When specifying the \code{fullColorRange}, the lowest and highest 'colors' in \code{generateColors} are anchored to the minimum and maximum values of \code{fullColorRange}.
}
  \item{color}{
  When no colors are automatically generated, all diamonds will have this color.
}
  \item{lineColor}{
  If NA, lines will have the same colors as the diamonds' fill. If not NA, must be a valid color, which is then used as line color. Note that e.g. \code{linetype} and \code{color} can be used as well, which will be passed on to \code{\link{geom_polygon}}.
}
  \item{otherAxisCol}{
  A vector of values, or the index of the column in the dataframe, that specifies the values for the Y axis of the diamonds. This should normally just be a vector of consecutive integers.
}
  \item{autoSize}{
  Whether to make the height of each diamond conditional upon
  its length (the width of the confidence interval).
}
  \item{fixedSize}{
  If not using relative heights, \code{fixedSize} determines
  the height to use.
}
  \item{\dots}{
  Any additional arguments are passed to \code{\link{geom_polygon}}. This can be used to set, for example, the \code{alpha} value of the diamonds. Additional arguments for \code{rawDataDiamondLayer} are passed on to \code{\link{geom_jitter}}.
}
  \item{values}{
  A vector of 2 or more values that are used to construct the diamond coordinates. If three values are provided, the middle one becomes the diamond's center. If two, four, or more values are provided, the median becomes the diamond's center.
}
  \item{otherAxisValue}{
  The value on the other axis to use to compute the coordinates; this will be the Y axis value of the points of the diamond (if \code{direction} is 'horizontal') or the X axis value (if \code{direction} is 'vertical').
}
  \item{direction}{
  Whether the diamonds should be constructed horizontally or vertically.
}
  \item{items}{
  The items from the dataframe to include in the diamondplot or dataframe.
}
  \item{labels}{
  The item labels to add to the dataframe.
}
  \item{decreasing}{
  Whether to sort the items (rows) in the dataframe decreasing (TRUE),
  increasing (FALSE), or not at all (NULL).
}
  \item{conf.level}{
  The confidence of the confidence intervals.
}
  \item{itemOrder}{
  Order of the items to use (if not sorting).
}
  \item{dataAlpha}{
  This determines the alpha (transparency) of the data points.
}
  \item{dataColor}{
  The color of the data points.
}
  \item{jitterWidth}{
  How much to jitter the individual datapoints horizontally.
}
  \item{jitterHeight}{
  How much to jitter the individual datapoints vertically.
}
  \item{size}{
  The size of the data points.
}
}
\value{
\code{ggDiamondLayer} returns a \code{\link{ggplot}} \code{\link{geom_polygon}} object, which can then be used in \code{\link{ggplot}} plots (as \code{\link{diamondPlot}} does).

\code{diamondCoordinates} returns a set of four coordinates that together specify a diamond.

\code{varsToDiamondPlotDf} returns a dataframe of diamondCoordinates.

\code{rawDataDiamondLayer} returns a \code{\link{geom_jitter}} object.

}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{meansDiamondPlot}}, \code{\link{meanSDtoDiamondPlot}}, \code{\link{factorLoadingDiamondCIplot}}, \code{\link{diamondPlot}}
}
\examples{
\dontrun{
### (Don't run this example as a test, because we
###  need the ggplot function which isn't part of
###  this package.)

### The coordinates for a simple diamond
diamondCoordinates(values = c(1,2,3));

### Plot this diamond
ggplot() + ggDiamondLayer(data.frame(1,2,3));
}
}
\keyword{ hplot }
