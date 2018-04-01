\name{genlog}
\alias{genlog}
\alias{genlogCompleteStartValues}
\alias{ggGenLogPlot}
\title{
Generalized Logistic Analysis
}
\description{
This function implements the generalized logistic analysis introduced in Verboon & Peters (2017). This analysis fits a logistic function (i.e. a sigmoid) to a data series. This is useful when analysing single case designs. The function enables easy customization of the main plot elements and easy saving of the plot with anti-aliasing. \code{ggGenLogPlot} does most of the plotting, and can be useful when trying to figure out sensible starting and boundary/constraint values. \code{genlogCompleteStartValues} tries to compute sensible starting and boundary/constraint values based on the data.
}
\usage{
genlog(data,
       timeVar = 1,
       yVar = 2,
       phaseVar = NULL,
       baselineMeasurements = NULL,
       yRange = NULL,
       startX = NULL,
       startBase = NULL,
       startTop = NULL,
       startGrowthRate = NULL,
       startV = 1,
       changeInitiationBounds = NULL,
       growthRateBounds = c(-2, 2),
       baseMargin = c(0, 3),
       topMargin = c(-3, 0),
       baseBounds = NULL,
       topBounds = NULL,
       vBounds = c(1, 1),
       colors = list(bounds = viridis(4)[4],
                     curve = viridis(4)[3],
                     mid = viridis(4)[2],
                     intervention = viridis(4)[1],
                     points = "black"),
       alphas = list(outsideRange = .2,
                     bounds = 0,
                     points = .5,
                     mid = 0),
       theme = theme_minimal(),
       pointSize = 2,
       pointAlpha = 0.5,
       lineSize = 0.5,
       initialValuesLineType = "blank",
       curveSizeMultiplier = 2,
       showPlot = TRUE,
       plotLabs = NULL,
       outputFile = NULL,
       outputWidth = 16,
       outputHeight = 16,
       ggsaveParams = list(units = "cm",
                           dpi = 300,
                           type = "cairo"))
}
\arguments{
  \item{data}{
  The dataframe containing the variables for the analysis.
}
  \item{timeVar}{
  The name of the variable containing the measurement moments (or an index of measurement moments). An index can also be specified, and assumed to be 1 if omitted.
}
  \item{yVar}{
  The name of the dependent variable. An index can also be specified, and assumed to be 2 if omitted.
}
  \item{phaseVar}{
  The variable containing the phase of each measurement. Note that this normally should only have two possible values.
}
  \item{baselineMeasurements}{
  If no phaseVar is specified, \code{baselineMeasurements} can be used to specify the number of baseline measurements, which is then used to construct the \code{phaseVar} dummy variable.
}
  \item{yRange}{
  This can be used to manually specify the possible values that the dependent variable can take. If no \code{startBase} and \code{startTop} are specified, the range of the dependent variable is used instead.
}
  \item{startX, startBase, startTop, startGrowthRate, startV}{
  The starting values used when estimating the sigmoid using \code{minpack.lm}'s \code{\link{nlsLM}} function. \code{startX} specifies the starting value to use for the measurement moment when the change is fastest (i.e. the slope of the sigmoid has the largest value); \code{startBase} and \code{startTop} specify the starting values to use for the base (floor) and top (ceiling), the plateaus of relative stability between which the sigmoid described the shift; \code{startGrowthRate} specifies the starting value for the growth rate; and \code{startV} specifies the starting value for the \emph{v} parameter.
}
  \item{changeInitiationBounds, growthRateBounds, baseMargin, topMargin, baseBounds, topBounds, vBounds}{
  These values specify constraints to respect when estimating the parameters of the sigmoid function using \code{minpack.lm}'s \code{\link{nlsLM}}. \code{changeInitiationBounds} specifies between which values the initiation of the shift must occur; \code{growthRateBounds} describes the bounds constraining the possible values for the growth rate; \code{baseBounds} and \code{topBounds} specify the constraints for possible values for the base (floor) and top (ceiling), the plateaus of relative stability between which the sigmoid described the shift; and if these are not specified, \code{baseMargin} and \code{topMargin} are used in combination with the range of the dependent variable to set these bounds (also see \code{yRange}); and finally, \code{vBounds} specifies the possible values that constrain the \emph{v} parameter.
}
  \item{colors}{
  The colors to use for the different plot elements.
}
  \item{alpha}{
  The alpha values (transparency, or rather, 'obliqueness', with 0 indicating full transparency and 1 indicating full visibility) to use for the different plot elements.
}
  \item{theme}{
  The theme to use in the plot.
}
  \item{pointSize,lineSize }{
  The sizes of points and lines in the plot.
}
  \item{pointAlpha}{
  The alpha channel (transparency, or rather, 'opaqueness') of the points.
}
  \item{initialValuesLineType}{
  The line type to use for the initial values; by default set to \code{"blank"} for \code{genlog}, to hide them, and to \code{"dashed"} for ggGenLogPlot.
}
  \item{curveSizeMultiplier}{
  A multiplyer for the curve size compared to the other lines (e.g. specify '2' to have a curve of twice the size).
}
  \item{showPlot}{
  Whether to show the plot or not.
}
  \item{plotLabs}{
  A list with arguments to the \code{\link{ggplot2}} \code{\link{labs}} function, which can be used to conveniently set plot labels.
}
  \item{outputFile}{
  If not \code{NULL}, the path and filename specifying where to save the plot.
}
  \item{outputWidth, outputHeight}{
  The dimensions of the plot when saving it (in units specified in \code{ggsaveParams}).
}
  \item{ggsaveParams}{
  The parameters to use when saving the plot, passed on to \code{\link{ggsave}}.
}
}
\details{
For details, see Verboon & Peters (2017).
}
\value{
Mainly, this function prints its results, but it also returns them in an object containing three lists:
  \item{input}{The arguments specified when calling the function}
  \item{intermediate}{Intermediat objects and values}
  \item{output}{The results such as the plot.}
}
\references{
Verboon, P. & Peters, G.-J. Y. (2018) Applying the generalised logistic model in single case designs: modelling treatment-induced shifts. \emph{PsyArXiv} \url{https://doi.org/10.17605/osf.io/ad5eh}
}
\author{
Peter Verboon & Gjalt-Jorn Peters (both at the Open University of the Netherlands)

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{genlogFunction}}
}
\examples{
### Load dataset
data(Singh);

### Extract Jason
dat <- Singh[Singh$tier==1, ];

### Conduct piecewise regression analysis
genlog(dat,
       timeVar='time',
       yVar='score_physical',
       phaseVar='phase');

}
\keyword{ hplot }
\keyword{ models }
\keyword{ htest }
