\name{associationsDiamondPlot}
\alias{associationsDiamondPlot}
\alias{associationsToDiamondPlotDf}
\title{
A diamondplot with confidence intervals for associations
}
\description{
This function produces is a diamondplot that plots the confidence intervals for
associations between a number of covariates and a criterion. It currently
only supports the Pearson's r effect size metric; other effect sizes are
converted to Pearson's r.

associationsToDiamondPlotDf is a helper function that produces the required
dataframe.
}
\usage{
associationsDiamondPlot(dat, covariates, criteria,
                        labels = NULL,
                        criteriaLabels = NULL,
                        decreasing=NULL,
                        sortBy=NULL,
                        conf.level=.95,
                        criteriaColors = brewer.pal(8, 'Set1'),
                        criterionColor = 'black',
                        returnLayerOnly = FALSE,
                        esMetric = 'r',
                        multiAlpha=.33,
                        singleAlpha = 1,
                        showLegend=TRUE,
                        xlab="Effect size estimates",
                        ylab="",
                        theme=theme_bw(),
                        lineSize = 1,
                        outputFile = NULL,
                        outputWidth = 10,
                        outputHeight = 10,
                        ggsaveParams = list(units='cm',
                                            dpi=300,
                                            type="cairo"),
                        ...)
                        
associationsToDiamondPlotDf(dat, covariates, criterion, labels = NULL,
                            decreasing = NULL, conf.level = 0.95,
                            esMetric = "r")
}
\arguments{
  \item{dat}{
  The dataframe containing the relevant variables.
}
  \item{covariates}{
  The covariates: the list of variables to associate to the criterion or criteria,
  usually the predictors.
}
  \item{criteria, criterion}{
  The criteria, usually the dependent variables; one criterion (one dependent variable) can also be specified of course. The helper function \code{associationsToDiamondPlotDf} always accepts only one criterion.
}
  \item{labels}{
  The labels for the covariates, for example the questions that were
  used (as a character vector).
}
  \item{criteriaLabels}{
  The labels for the criteria (in the legend).
}
  \item{decreasing}{
  Whether to sort the covariates by the point estimate of the effect size
  of their association with the criterion. Use \code{NULL} to not sort
  at all, \code{TRUE} to sort in descending order, and \code{FALSE} to
  sort in ascending order.
}
  \item{sortBy}{
  When specifying multiple criteria, this can be used to indicate by which criterion the items should be sorted (if they should be sorted).
}
  \item{conf.level}{
  The confidence of the confidence intervals.
}
  \item{criteriaColors, criterionColor}{
  The colors to use for the different associations can be specified in \code{criteriaColors}. This should be a vector of valid colors with at least as many elements as criteria are specified in \code{criteria}. If only one criterion is specified, the color in \code{criterionColor} is used.
}
  \item{returnLayerOnly}{
  Whether to return the entire object that is generated, or just the
  resulting ggplot2 layer.
}
  \item{esMetric}{
  The effect size metric to plot - currently, only 'r' is supported, and
  other values will return an error.
}
  \item{multiAlpha, singleAlpha}{
  The transparency (alpha channel) value of the diamonds for each association can be specified in \code{multiAlpha}, and if only one criterion is specified, the alpha level of the diamonds can be specified in \code{singleAlpha}.
}
  \item{showLegend}{
  Whether to show the legend.
}
  \item{xlab, ylab}{
The label to use for the x and y axes (for \code{duoComparisonDiamondPlot}, must be vectors of two elements). Use \code{NULL} to not use a label.
}
  \item{theme}{
  The \code{\link{ggplot}} theme to use.
}
  \item{lineSize}{
  The thickness of the lines (the diamonds' strokes).
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
  Any additional arguments are passed to \code{\link{diamondPlot}} and
  eventually to \code{\link{ggDiamondLayer}}.
}
}
\details{
  This function can be used to quickly plot multiple confidence intervals.
}
\value{
  A plot.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{diamondPlot}}, \code{\link{ggDiamondLayer}}, \code{\link{CIBER}}
}
\examples{
### Simple diamond plot with correlations
### and their confidence intervals

associationsDiamondPlot(mtcars,
                        covariates=c('cyl', 'hp', 'drat', 'wt',
                                     'am', 'gear', 'vs', 'carb', 'qsec'),
                        criteria='mpg');

### Same diamond plot, but now with two criteria,
### and colouring the diamonds based on the
### correlation point estimates: a gradient
### is created where red is used for -1,
### green for 1 and blue for 0.

associationsDiamondPlot(mtcars,
                        covariates=c('cyl', 'hp', 'drat', 'wt',
                                     'am', 'gear', 'vs', 'carb', 'qsec'),
                        criteria=c('mpg', 'disp'),
                        generateColors=c("red", "blue", "green"),
                        fullColorRange=c(-1, 1));
}
\keyword{ hplot }
