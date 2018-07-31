#' Easy ggplot2 scatter plots
#' 
#' This function is intended to provide a very easy interface to generating
#' pretty (and pretty versatile) \code{\link{ggplot}} scatter plots.
#' 
#' Note that if \code{position} is set to 'jitter', unless \code{width} and/or
#' \code{height} is set to a non-zero value, there will still not be any
#' jittering.
#' 
#' @param x The variable to plot on the X axis.
#' @param y The variable to plot on the Y axis.
#' @param pointsize The size of the points in the scatterplot.
#' @param theme The theme to use.
#' @param regrLine Whether to show the regression line.
#' @param regrCI Whether to display the confidence interval around the
#' regression line.
#' @param regrLineCol The color of the regression line.
#' @param regrCIcol The color of the confidence interval around the regression
#' line.
#' @param regrCIalpha The alpha value (transparency) of the confidence interval
#' around the regression line.
#' @param width If \code{position} is 'jitter', the points are 'jittered': some
#' random noise is added to change their location slightly. In that case
#' 'width' can be set to determine how much the location should be allowed to
#' vary on the X axis.
#' @param height If \code{position} is 'jitter', the points are 'jittered':
#' some random noise is added to change their location slightly. In that case
#' 'height' can be set to determine how much the location should be allowed to
#' vary on the Y axis.
#' @param position Whether to 'jitter' the points (adding some random noise to
#' change their location slightly, used to prevent overplotting). Set to
#' 'jitter' to jitter the points.
#' @param xVarName,yVarName Can be used to manually specify the names of the
#' variables on the x and y axes.
#' @param \dots And additional arguments are passed to \code{\link{geom_point}}
#' or \code{\link{geom_jitter}} (if \code{jitter} is set to 'jitter').
#' @return A \code{\link{ggplot}} plot is returned.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{geom_point}}, \code{\link{geom_jitter}},
#' \code{\link{geom_smooth}}
#' @keywords hplot
#' @examples
#' 
#' 
#' ### A simple scatter plot
#' scatterPlot(mtcars$mpg, mtcars$hp);
#' 
#' ### The same scatter plot, now with a regression line
#' ### and its confidence interval added.
#' scatterPlot(mtcars$mpg, mtcars$hp, regrLine=TRUE, regrCI=TRUE);
#' 
#' 
#' @export scatterPlot
scatterPlot <- function(x, y, pointsize=3,
                        theme = theme_bw(),
                        regrLine = FALSE,
                        regrCI = FALSE,
                        regrLineCol = "blue",
                        regrCIcol = regrLineCol,
                        regrCIalpha = .25,
                        width = 0,
                        height = 0,
                        position="identity",
                        xVarName=NULL,
                        yVarName=NULL,
                        ...) {
  xVarName <- ifelse(is.null(xVarName),
                     extractVarName(deparse(substitute(x))),
                     xVarName);
  yVarName <- ifelse(is.null(yVarName),
                     extractVarName(deparse(substitute(y))),
                     yVarName);

  dat <- data.frame(x, y);
  names(dat) <- c(xVarName, yVarName);
  plot <- ggplot(dat, aes_string(xVarName, yVarName)) +
    theme;
  if (regrLine && regrCI) {
    plot <- plot + geom_smooth(method='lm', color = regrLineCol,
                               fill = regrCIcol, alpha = regrCIalpha,
                               na.rm=TRUE);
  } else if (regrLine) {
    plot <- plot + geom_smooth(method='lm', color = regrLineCol,
                               se=FALSE, na.rm=TRUE);
  }
  if (!is.null(position) && (tolower(position)=='identity') && (width==0) && (height==0)) {
    plot <- plot + geom_point(na.rm=TRUE,
                              size=pointsize,
                              ...);
  } else {
    plot <- plot + geom_jitter(na.rm=TRUE,
                               size=pointsize,
                               width = width,
                               height = height,
                               ...);
  }
  return(plot);
}
