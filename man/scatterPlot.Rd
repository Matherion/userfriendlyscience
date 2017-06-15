\name{scatterPlot}
\alias{scatterPlot}
\title{
Easy ggplot2 scatter plots
}
\description{
This function is intended to provide a very easy interface to generating pretty (and pretty versatile) \code{\link{ggplot}} scatter plots.
}
\usage{
scatterPlot(x, y, pointsize = 3,
            theme = theme_bw(),
            regrLine = FALSE, regrCI = FALSE,
            regrLineCol = "blue",
            regrCIcol = regrLineCol,
            regrCIalpha = 0.25,
            width = 0, height = 0,
            position = "identity",
            xVarName = NULL,
            yVarName = NULL, 
            ...)
}
\arguments{
  \item{x}{
  The variable to plot on the X axis.
}
  \item{y}{
  The variable to plot on the Y axis.
}
  \item{pointsize}{
  The size of the points in the scatterplot.
}
  \item{theme}{
  The theme to use.
}
  \item{regrLine}{
  Whether to show the regression line.
}
  \item{regrCI}{
  Whether to display the confidence interval around the regression line.
}
  \item{regrLineCol}{
  The color of the regression line.
}
  \item{regrCIcol}{
  The color of the confidence interval around the regression line.
}
  \item{regrCIalpha}{
  The alpha value (transparency) of the confidence interval around the regression line.
}
  \item{width}{
  If \code{position} is 'jitter', the points are 'jittered': some random noise is added to change their location slightly. In that case 'width' can be set to determine how much the location should be allowed to vary on the X axis.
}
  \item{height}{
  If \code{position} is 'jitter', the points are 'jittered': some random noise is added to change their location slightly. In that case 'height' can be set to determine how much the location should be allowed to vary on the Y axis.
}
  \item{position}{
  Whether to 'jitter' the points (adding some random noise to change their location slightly, used to prevent overplotting). Set to 'jitter' to jitter the points.
}
  \item{xVarName, yVarName}{
  Can be used to manually specify the names of the variables on the x and y axes.
}
  \item{\dots}{
  And additional arguments are passed to \code{\link{geom_point}} or \code{\link{geom_jitter}} (if \code{jitter} is set to 'jitter').
}
}
\details{
Note that if \code{position} is set to 'jitter', unless \code{width} and/or \code{height} is set to a non-zero value, there will still not be any jittering.
}
\value{
A \code{\link{ggplot}} plot is returned.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{geom_point}}, \code{\link{geom_jitter}}, \code{\link{geom_smooth}}
}
\examples{

### A simple scatter plot
scatterPlot(mtcars$mpg, mtcars$hp);

### The same scatter plot, now with a regression line
### and its confidence interval added.
scatterPlot(mtcars$mpg, mtcars$hp, regrLine=TRUE, regrCI=TRUE);

}
\keyword{ hplot }
