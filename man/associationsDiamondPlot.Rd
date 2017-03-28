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
associationsDiamondPlot(dat, covariates, criterion, labels = NULL,
                        decreasing = NULL, conf.level = 0.95,
                        diamondColors = NULL,
                        returnLayerOnly = FALSE, esMetric = "r", ...)
                        
associationsToDiamondPlotDf(dat, covariates, criterion, labels = NULL,
                            decreasing = NULL, conf.level = 0.95,
                            esMetric = "r")
}
\arguments{
  \item{dat}{
  The dataframe containing the relevant variables.
}
  \item{covariates}{
  The covariates: the list of variables to associate to the criterion,
  usually the predictors.
}
  \item{criterion}{
  The criterion, usually the dependent variable.
}
  \item{labels}{
  The labels for the covariates, for example the questions that were
  used (as a character vector).
}
  \item{decreasing}{
  Whether to sort the covariates by the point estimate of the effect size
  of their association with the criterion. Use \code{NULL} to not sort
  at all, \code{TRUE} to sort in descending order, and \code{FALSE} to
  sort in ascending order.
}
  \item{conf.level}{
  The confidence of the confidence intervals.
}
  \item{diamondColors}{
  A vector with colors for the diamonds can be passed; has to be the
  exact same length as \code{covariates}.
}
  \item{returnLayerOnly}{
  Whether to return the entire object that is generated, or just the
  resulting ggplot2 layer.
}
  \item{esMetric}{
  The effect size metric to plot - currently, only 'r' is supported, and
  other values will return an error.
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
### Simple diamond plot with correlations and their confidence intervals
associationsDiamondPlot(mtcars,
                        covariates=c('cyl', 'disp', 'hp', 'drat', 'wt',
                                     'am', 'gear', 'vs', 'carb', 'qsec'),
                        criterion='mpg');

### Same diamong plot, but now colouring the diamonds based on the
### correlation point estimates: a gradient is created where red is used
### for -1, green for 1 and blue for 0.
associationsDiamondPlot(mtcars,
                        covariates=c('cyl', 'disp', 'hp', 'drat', 'wt',
                                     'am', 'gear', 'vs', 'carb', 'qsec'),
                        criterion='mpg',
                        generateColors=c("red", "blue", "green"),
                        fullColorRange=c(-1, 1));
}
\keyword{ hplot }
