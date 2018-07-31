#' Basic ggplot2 diamond plot layer construction functions
#' 
#' These functions are used by \code{\link{diamondPlot}} to construct a diamond
#' plot. It's normally not necessary to call this function directly: instead,
#' use \code{\link{meansDiamondPlot}}, \code{\link{meanSDtoDiamondPlot}}, and
#' \code{\link{factorLoadingDiamondCIplot}}.
#' 
#' 
#' @aliases ggDiamondLayer diamondCoordinates varsToDiamondPlotDf
#' rawDataDiamondLayer
#' @param data,dat A dataframe (or matrix) containing lower bounds, centers
#' (e.g. means), and upper bounds of intervals (e.g. confidence intervals) for
#' \code{ggDiamondLayer} or items and raw data for \code{varsToDiamondPlotDf}
#' and \code{rawDataDiamondLayer}.
#' @param ciCols The columns in the dataframe with the lower bounds, centers
#' (e.g. means), and upper bounds (in that order).
#' @param colorCol The column in the dataframe containing the colors for each
#' diamond, or a vector with colors (with as many elements as the dataframe has
#' rows).
#' @param generateColors A vector with colors to use to generate a gradient.
#' These colors must be valid arguments to \code{\link{colorRamp}} (and
#' therefore, to \code{\link{col2rgb}}).
#' @param fullColorRange When specifying a gradient using
#' \code{generateColors}, it is usually desirable to specify the minimum and
#' maximum possible value corresponding to the outer anchors of that gradient.
#' For example, when plotting numbers from 0 to 100 using a gradient from 'red'
#' through 'orange' to 'green', none of the means may actually be 0 or 100; the
#' lowest mean may be, for example, 50. If no \code{fullColorRange} is
#' specified, the diamond representing that lowest mean of 50 wil be red, not
#' orange. When specifying the \code{fullColorRange}, the lowest and highest
#' 'colors' in \code{generateColors} are anchored to the minimum and maximum
#' values of \code{fullColorRange}.
#' @param color When no colors are automatically generated, all diamonds will
#' have this color.
#' @param lineColor If NA, lines will have the same colors as the diamonds'
#' fill. If not NA, must be a valid color, which is then used as line color.
#' Note that e.g. \code{linetype} and \code{color} can be used as well, which
#' will be passed on to \code{\link{geom_polygon}}.
#' @param otherAxisCol A vector of values, or the index of the column in the
#' dataframe, that specifies the values for the Y axis of the diamonds. This
#' should normally just be a vector of consecutive integers.
#' @param autoSize Whether to make the height of each diamond conditional upon
#' its length (the width of the confidence interval).
#' @param fixedSize If not using relative heights, \code{fixedSize} determines
#' the height to use.
#' @param \dots Any additional arguments are passed to
#' \code{\link{geom_polygon}}. This can be used to set, for example, the
#' \code{alpha} value of the diamonds. Additional arguments for
#' \code{rawDataDiamondLayer} are passed on to \code{\link{geom_jitter}}.
#' @param values A vector of 2 or more values that are used to construct the
#' diamond coordinates. If three values are provided, the middle one becomes
#' the diamond's center. If two, four, or more values are provided, the median
#' becomes the diamond's center.
#' @param otherAxisValue The value on the other axis to use to compute the
#' coordinates; this will be the Y axis value of the points of the diamond (if
#' \code{direction} is 'horizontal') or the X axis value (if \code{direction}
#' is 'vertical').
#' @param direction Whether the diamonds should be constructed horizontally or
#' vertically.
#' @param items The items from the dataframe to include in the diamondplot or
#' dataframe.
#' @param labels The item labels to add to the dataframe.
#' @param decreasing Whether to sort the items (rows) in the dataframe
#' decreasing (TRUE), increasing (FALSE), or not at all (NULL).
#' @param conf.level The confidence of the confidence intervals.
#' @param itemOrder Order of the items to use (if not sorting).
#' @param dataAlpha This determines the alpha (transparency) of the data
#' points.
#' @param dataColor The color of the data points.
#' @param jitterWidth How much to jitter the individual datapoints
#' horizontally.
#' @param jitterHeight How much to jitter the individual datapoints vertically.
#' @param size The size of the data points.
#' @return \code{ggDiamondLayer} returns a \code{\link{ggplot}}
#' \code{\link{geom_polygon}} object, which can then be used in
#' \code{\link{ggplot}} plots (as \code{\link{diamondPlot}} does).
#' 
#' \code{diamondCoordinates} returns a set of four coordinates that together
#' specify a diamond.
#' 
#' \code{varsToDiamondPlotDf} returns a dataframe of diamondCoordinates.
#' 
#' \code{rawDataDiamondLayer} returns a \code{\link{geom_jitter}} object.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{meansDiamondPlot}}, \code{\link{meanSDtoDiamondPlot}},
#' \code{\link{factorLoadingDiamondCIplot}}, \code{\link{diamondPlot}}
#' @keywords hplot
#' @examples
#' 
#' \dontrun{
#' ### (Don't run this example as a test, because we
#' ###  need the ggplot function which isn't part of
#' ###  this package.)
#' 
#' ### The coordinates for a simple diamond
#' diamondCoordinates(values = c(1,2,3));
#' 
#' ### Plot this diamond
#' ggplot() + ggDiamondLayer(data.frame(1,2,3));
#' }
#' 
#' @export ggDiamondLayer
ggDiamondLayer <- function(data,
                           ciCols=1:3,
                           colorCol=NULL,
                           generateColors = NULL,
                           fullColorRange = NULL,
                           color="black",
                           lineColor=NA,
                           otherAxisCol=1:nrow(data),
                           autoSize=NULL,
                           fixedSize=.15,
                           direction="horizontal",
                           ...) {

  ### Set column with y axis values
  if (length(otherAxisCol) > 1) {
    data[, 'otherAxisValues'] <- otherAxisCol;
    otherAxisCol <- 'otherAxisValues';
  }

  ### If we need to generate colours, do so and set color column
  if (!is.null(generateColors)) {
    # data[, ncol(data) + 1] <- colorRampPalette(generateColours)(nrow(data));
    # colourCol <- ncol(data);

    if (is.null(fullColorRange)) {
      fullColorRange <- c(min(unlist(data[[ciCols[2]]])),
                          max(unlist(data[[ciCols[2]]])));
    }
    data[, ncol(data) + 1] <- scales::rescale(unlist(data[[ciCols[2]]]),
                                              to = c(0,1),
                                              from = fullColorRange);
    colorPositionCol <- ncol(data);
    colorPaletteFunction <- colorRamp(generateColors);

    data[!is.na(data[, colorPositionCol]), ncol(data) + 1] <-
      rgb(colorPaletteFunction(data[!is.na(data[, colorPositionCol]), colorPositionCol]) / 256);
    colorCol <- ncol(data);

  }

  return(apply(data, 1, function(x,
                                 cCol=colorCol,
                                 aSize=autoSize,
                                 fSize = fixedSize) {
    tmpDf <- data.frame(diamondCoordinates(as.numeric(unlist(x[ciCols])),
                                           otherAxisValue=as.numeric(x[[otherAxisCol]]),
                                           autoSize = aSize,
                                           fixedSize = fSize,
                                           direction=direction));

    if (is.null(cCol)) {
      ### If no color column is passed, just use the general color
      return(geom_polygon(tmpDf,
                          mapping=aes(x=x, y=y),
                          fill=color,
                          color=ifelse(is.na(lineColor), color, lineColor), ...));
    } else if (!is.numeric(cCol) && all(unlist(areColors(cCol)))) {
      ### If a specific color is passed, use that
      return(geom_polygon(tmpDf,
                          mapping=aes(x=x, y=y),
                          fill=cCol,
                          color = ifelse(is.na(lineColor), cCol, lineColor), ...));
    } else {
      ### Otherwise, a column with a color is passed, so extract & use that
      return(geom_polygon(tmpDf,
                          mapping=aes(x=x, y=y),
                          fill=x[[cCol]],
                          color = ifelse(is.na(lineColor), x[[cCol]], lineColor), ...));
    }
  }));
}
