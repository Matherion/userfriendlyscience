\name{biAxisDiamondPlot}
\alias{biAxisDiamondPlot}
\title{
Diamondplot with two Y axes
}
\description{
This is basically a \code{\link{meansDiamondPlot}}, but extended to
allow specifying subquestions and anchors at the left and right side.
This is convenient for psychological questionnaires when the anchors
or dimensions were different from item to item. This function is used
to function the left panel of the \code{\link{CIBER}} plot.
}
\usage{
biAxisDiamondPlot(dat, items = NULL,
                  leftAnchors = NULL, rightAnchors = NULL,
                  subQuestions = NULL,
                  decreasing = NULL, conf.level = 0.95,
                  showData = TRUE, dataAlpha = 0.1,
                  dataColor = "#444444", diamondColors = NULL,
                  jitterWidth = 0.45, jitterHeight = 0.45,
                  xBreaks = NULL, xLabels = NA,
                  xAxisLab = paste0("Scores and ",
                                    round(100 * conf.level, 2),
                                    "\% CIs"),
                  drawPlot = TRUE, returnPlotOnly = TRUE,
                  baseSize = 1, dotSize = baseSize,
                  baseFontSize = 10 * baseSize,
                  theme = theme_bw(base_size = baseFontSize), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
  The dataframe containing the variables.
}
  \item{items}{
  The variables to include.
}
  \item{leftAnchors}{
  The anchors to display on the left side of the left hand panel. If the
  items were measured with one variable each, this can be used to
  show the anchors that were used for the respective scales. Must have the
  same length as \code{items}.
}
  \item{rightAnchors}{
  The anchors to display on the left side of the left hand panel. If the
  items were measured with one variable each, this can be used to
  show the anchors that were used for the respective scales. Must have the
  same length as \code{items}.
}
  \item{subQuestions}{
  The subquestions used to measure each item. This can also be
  used to provide pretty names for the variables if the items
  were not measured by one question each. Must have the same length as
  \code{items}.
}
  \item{decreasing}{
  Whether to sort the items. Specify \code{NULL} to not sort
  at all, \code{TRUE} to sort in descending order, and \code{FALSE} to
  sort in ascending order.
}
  \item{conf.level}{
  The confidence levels for the confidence intervals.
}
  \item{showData}{
  Whether to show the individual datapoints.
}
  \item{dataAlpha}{
  The alpha level (transparency) of the individual datapoints.
  Value between 0 and 1, where 0 signifies complete
  transparency (i.e. invisibility) and 1 signifies complete 'opaqueness'.
}
  \item{dataColor}{
  The color to use for the individual datapoints.
}
  \item{diamondColors}{
  The colours to use for the diamonds. If NULL,
  the \code{generateColors} argument can be used
  which will then be passed to \code{\link{diamondPlot}}.
}
  \item{jitterWidth}{
  How much to jitter the individual datapoints horizontally.
}
  \item{jitterHeight}{
  How much to jitter the individual datapoints vertically.
}
  \item{xBreaks}{
  Which breaks to use on the X axis (can be useful to override
  \code{\link{ggplot}}'s defaults).
}
  \item{xLabels}{
  Which labels to use for those breaks (can be useful to override
  \code{\link{ggplot}}'s defaults; especially useful in combination
  with \code{xBreaks} of course).
}
  \item{xAxisLab}{
  Axis label for the X axis.
}
  \item{drawPlot}{
  Whether to draw the plot, or only return it.
}
  \item{returnPlotOnly}{
  Whether to return the entire object that is generated (including all
  intermediate objects) or only the plot.
}
  \item{baseSize}{
  This can be used to efficiently change the size of most plot elements.
}
  \item{dotSize}{
  This is the size of the points used to show the individual data points
  in the left hand plot.
}
  \item{baseFontSize}{
  This can be used to set the font size separately from the \code{baseSize}.
}
  \item{theme}{
  This is the theme that is used for the plots.
}
  \item{\dots}{
  These arguments are passed on to \code{\link{diamondPlot}}.
}
}
\details{
This is a diamondplot that can be used for items/questions where
the anchors of the response scales could be different for every
item. For the rest, it is very similar to \code{\link{meansDiamondPlot}}.
}
\value{
Either just a plot (a \code{\link{gtable}} object) or an object
with all produced objects and that plot.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{CIBER}}, \code{\link{associationsDiamondPlot}}
}
\examples{
biAxisDiamondPlot(dat=mtcars,
                  items=c('cyl', 'wt'),
                  subQuestions=c('cylinders', 'weight'),
                  leftAnchors=c('few', 'light'),
                  rightAnchors=c('many', 'heavy'),
                  xBreaks=0:8);
}
\keyword{ hplot }
