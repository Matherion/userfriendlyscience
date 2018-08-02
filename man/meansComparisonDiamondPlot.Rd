\name{meansComparisonDiamondPlot}
\alias{meansComparisonDiamondPlot}
\alias{duoComparisonDiamondPlot}
\title{
meansComparisonDiamondPlot and duoComparisonDiamondPlot
}
\description{
These are two diamond plot functions to conveniently make diamond plots to compare subgroups or different samples. They are both based on a univariate diamond plot where colors are used to distinguish the data points and diamonds of each subgroup or sample. The means comparison diamond plot produces only this plot, while the duo comparison diamond plot combines it with a diamond plot visualising the effect sizes of the associations. The latter currently only works for two subgroups or samples, while the simple meansComparisonDiamondPlot also works when comparing more than two sets of datapoints. These functions are explained more in detail in Peters (2017).
}
\usage{
meansComparisonDiamondPlot(dat, items = NULL,
                           compareBy = NULL,
                           labels = NULL,
                           compareByLabels = NULL,
                           decreasing = NULL,
                           sortBy = NULL,
                           conf.level = 0.95,
                           showData = TRUE,
                           dataAlpha = 0.1, dataSize = 3,
                           comparisonColors = brewer.pal(8, "Set1"),
                           alpha = 0.33,
                           jitterWidth = 0.5, jitterHeight = 0.4,
                           xlab = "Scores and means",
                           ylab = NULL,
                           theme = theme_bw(),
                           showLegend = TRUE,
                           lineSize = 1,
                           xbreaks = "auto",
                           outputFile = NULL,
                           outputWidth = 10,
                           outputHeight = 10,
                           ggsaveParams = list(units='cm',
                                               dpi=300,
                                               type="cairo"),
                           ...)
                           
duoComparisonDiamondPlot(dat, items = NULL,
                         compareBy = NULL,
                         labels = NULL,
                         compareByLabels = NULL,
                         decreasing = NULL,
                         conf.level = c(0.95, 0.95),
                         showData = TRUE,
                         dataAlpha = 0.1,
                         dataSize = 3,
                         comparisonColors = brewer.pal(8, "Set1"),
                         associationsColor = "grey",
                         alpha = 0.33,
                         jitterWidth = 0.5, jitterHeight = 0.4,
                         xlab = c("Scores and means", "Effect size estimates"),
                         ylab = c(NULL, NULL),
                         theme = theme_bw(),
                         showLegend = TRUE,
                         lineSize = 1,
                         drawPlot = TRUE,
                         xbreaks = "auto",
                         outputFile = NULL,
                         outputWidth = 10,
                         outputHeight = 10,
                         ggsaveParams = list(units='cm',
                                             dpi=300,
                                             type="cairo"),
                         ...)
}
\arguments{
  \item{dat}{
The dataframe containing the relevant variables.
}
  \item{items}{
The variables to plot (on the y axis).
}
  \item{compareBy}{
The variable by which to compare (i.e. the variable indicating to which subgroup or sample a row in the dataframe belongs).
}
  \item{labels}{
The labels to use on the y axis; these values will replace the variable names in the dataframe (specified in \code{items}).
}
  \item{compareByLabels}{
The labels to use to replace the value labels of the \code{compareBy} variable.
}
  \item{decreasing}{
Whether to sort the variables by their mean values (\code{NULL} to not sort, \code{TRUE} to sort in descending order (i.e. items with lower means are plotted more to the bottom), and \code{FALSE} to sort in ascending order (i.e. items with lower means are plotted more to the top).
}
  \item{sortBy}{
If the variables should be sorted (see \code{decreasing}), this variable specified which subgroup should be sorted by. Therefore, the value specified here must be a value label ('level label') of the \code{comparisonBy} variable.
}
  \item{conf.level}{
The confidence level of the confidence intervals specified by the diamonds for the means (for \code{meansComparisonDiamondPlot}) and for both the means and effect sizes (for \code{duoComparisonDiamondPlot}).
}
  \item{showData}{
Whether to plot the data points.
}
  \item{dataAlpha}{
The transparency (alpha channel) value for the data points: a value between 0 and 1, where 0 denotes complete transparency and 1 denotes complete opacity.
}
  \item{dataSize}{
The size of the data points.
}
  \item{comparisonColors}{
The colors to use for the different subgroups or samples. This should be a vector of valid colors with at least as many elements as sets of data points that should be plotted.
}
  \item{associationsColor}{
For \code{duoComparisonDiamondPlot}, the color to use to plot the effect sizes in the right-hand plot.
}
  \item{alpha}{
The alpha channel (transparency) value for the diamonds: a value between 0 and 1, where 0 denotes complete transparency and 1 denotes complete opacity.
}
  \item{jitterWidth, jitterHeight}{
How much noise to add to the data points (to prevent overplotting) in the horizontal (x axis) and vertical (y axis) directions.
}
  \item{xlab, ylab}{
The label to use for the x and y axes (for \code{duoComparisonDiamondPlot}, must be vectors of two elements). Use \code{NULL} to not use a label.
}
  \item{theme}{
  The theme to use for the plots.
}
  \item{showLegend}{
  Whether to show the legend (which color represents which subgroup/sample).
}
  \item{lineSize}{
  The thickness of the lines (the diamonds' strokes).
}
  \item{drawPlot}{
  Whether to draw the plot, or only (invisibly) return it.
}
  \item{xbreaks}{
  Where the breaks (major grid lines, ticks, and labels) on the x axis should be.
}
  \item{outputFile}{
  A file to which to save the plot.
}
  \item{outputWidth, outputHeight}{
  Width and height of saved plot (specified in centimeters by default, see \code{ggsaveParams}).
}
  \item{ggsaveParams}{
  Parameters to pass to ggsave when saving the plot.
}
  \item{\dots}{
  Any additional arguments are passed to \code{\link{diamondPlot}} by \code{meansComparisonDiamondPlot} and to both \code{meansComparisonDiamondPlot} and \code{\link{associationsDiamondPlot}} by \code{duoComparisonDiamondPlot}.
}
}
\details{
  These functions are explained in Peters (2017).
}
\value{
Diamond plots: a \code{\link{ggplot}} by \code{meansComparisonDiamondPlot}, and a \code{\link{gtable}} by \code{duoComparisonDiamondPlot}.
}
\references{
Peters, G.-J. Y. (2017). Diamond Plots: a tutorial to introduce a visualisation tool that facilitates interpretation and comparison of multiple sample estimates while respecting their inaccuracy. \emph{PsyArXiv.} https://doi.org/10.17605/OSF.IO/9W8YV
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{diamondPlot}}, \code{\link{meansDiamondPlot}}, \code{\link{CIBER}}
}
\examples{
meansComparisonDiamondPlot(mtcars,
                           items=c('disp', 'hp'),
                           compareBy='vs',
                           xbreaks=c(100,200, 300, 400));
meansComparisonDiamondPlot(chickwts,
                           items='weight',
                           compareBy='feed',
                           xbreaks=c(100,200,300,400),
                           showData=FALSE);
duoComparisonDiamondPlot(mtcars,
                         items=c('disp', 'hp'),
                         compareBy='vs',
                         xbreaks=c(100,200, 300, 400));
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
