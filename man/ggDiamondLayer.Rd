\name{ggDiamondLayer}
\alias{ggDiamondLayer}
\alias{diamondCoordinates}
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
               otherAxisCol = 1:nrow(data),
               autoSize = NULL,
               fixedSize = 0.15,
               ...)
diamondCoordinates(values,
                   otherAxisValue = 1,
                   direction = "horizontal",
                   autoSize = NULL,
                   fixedSize = 0.15)
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
  \item{generateColors}{
  A vector with colors to use to generate a gradient. These colors must be valid arguments to \code{\link{colorRamp}} (and therefore, to \code{\link{col2rgb}}).
}
  \item{fullColorRange}{
  When specifying a gradient using \code{generateColors}, it is usually desirable to specify the minimum and maximum possible value corresponding to the outer anchors of that gradient. For example, when plotting numbers from 0 to 100 using a gradient from 'red' through 'orange' to 'green', none of the means may actually be 0 or 100; the lowest mean may be, for example, 50. If no \code{fullColorRange} is specified, the diamond representing that lowest mean of 50 wil be red, not orange. When specifying the \code{fullColorRange}, the lowest and highest 'colors' in \code{generateColors} are anchored to the minimum and maximum values of \code{fullColorRange}.
}
  \item{color}{
  When no colors are automatically generated, all diamonds will have this color.
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
  Any additional arguments are passed to \code{\link{geom_polygon}}. This can be used to set, for example, the \code{alpha} value of the diamonds.
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
}
\value{
\code{ggDiamondLayer} returns a \code{\link{ggplot}} \code{\link{geom_polygon}} object, which can then be used in \code{\link{ggplot}} plots (as \code{\link{diamondPlot}} does).

\code{diamondCoordinates} returns a set of four coordinates that together specify a diamond.
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
