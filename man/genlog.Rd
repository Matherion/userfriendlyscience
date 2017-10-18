\name{genlog}
\alias{genlog}
\title{
Generalized Logistic Analysis
}
\description{
This function implements the generalized logistic analysis introduced in Verboon & Peters (2017). This analysis fits a logistic function (i.e. a sigmoid) to a data series. This is useful when analysing single case designs. The function enables easy customization of the main plot elements and easy saving of the plot with anti-aliasing.
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
       theme = theme_minimal(),
       pointSize = 2,
       pointAlpha = 0.5,
       lineSize = 0.5,
       curveSizeMultiplier = 2,
       showPlot = TRUE,
       outputFile = NULL,
       outputWidth = 16,
       outputHeight = 16,
       ggsaveParams = list(units = "cm",
                           dpi = 300,
                           type = "cairo"))
}
\arguments{
  \item{dat}{
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
%%     ~~Describe \code{yRange} here~~
}
  \item{startX}{
%%     ~~Describe \code{startX} here~~
}
  \item{startBase}{
%%     ~~Describe \code{startBase} here~~
}
  \item{startTop}{
%%     ~~Describe \code{startTop} here~~
}
  \item{startGrowthRate}{
%%     ~~Describe \code{startGrowthRate} here~~
}
  \item{startV}{
%%     ~~Describe \code{startV} here~~
}
  \item{changeInitiationBounds}{
%%     ~~Describe \code{changeInitiationBounds} here~~
}
  \item{growthRateBounds}{
%%     ~~Describe \code{growthRateBounds} here~~
}
  \item{baseMargin}{
%%     ~~Describe \code{baseMargin} here~~
}
  \item{topMargin}{
%%     ~~Describe \code{topMargin} here~~
}
  \item{baseBounds}{
%%     ~~Describe \code{baseBounds} here~~
}
  \item{topBounds}{
%%     ~~Describe \code{topBounds} here~~
}
  \item{vBounds}{
%%     ~~Describe \code{vBounds} here~~
}
  \item{colors}{
  The colors to use for the different plot elements.
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
  \item{curveSizeMultiplier}{
  A multiplyer for the curve size compared to the other lines (e.g. specify '2' to have a curve of twice the size).
}
  \item{showPlot}{
  Whether to show the plot or not.
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
Verboon, P. & Peters, G.-J. Y. (2017) Applying the generalised logistic model in SCD to deal with ceiling effects. \emph{PsyArXiv} http://INSERTLINK
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
