\name{piecewiseRegr}
\alias{piecewiseRegr}
\title{
Piecewise regression analysis
}
\description{
This function conducts a piecewise regression analysis and shows a plot illustrating the results. The function enables easy customization of the main plot elements and easy saving of the plot with anti-aliasing.
}
\usage{
piecewiseRegr(data,
              timeVar = 1,
              yVar = 2,
              phaseVar = NULL,
              baselineMeasurements = NULL,
              robust = FALSE,
              digits = 2,
              colors = list(pre = viridis(4)[1],
                            post = viridis(4)[4],
                            diff = viridis(4)[3],
                            intervention = viridis(4)[2],
                            points = "black"),
              theme = theme_minimal(),
              pointSize = 2,
              pointAlpha = 1,
              lineSize = 1,
              yRange=NULL,
              yBreaks = NULL,
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
  \item{robust}{
  Whether to use normal or robust linear regression.
}
  \item{digits}{
  The number of digits to show in the results.
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
  \item{yRange}{
  This can be used to manually specify the possible values that the dependent variable can take. If not provided, the observed range of the dependent variable values is used instead.
}
  \item{yBreaks}{
  If \code{NULL}, the \code{\link{pretty}} function is used to estimate the best breaks for the Y axis. If a value is supplied, this value is used as the size of intervals between the (floored) minimum and (ceilinged) maximum of \code{yRange} (e.g. if \code{yBreaks} is 1, a break point every integer; if 2 and the minimum is 1 and the maximum is 7, breaks at 1, 3, 5 and 7; etc).
}
  \item{pointAlpha}{
  The alpha channel (transparency, or rather, 'opaqueness') of the points.
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
\value{
Mainly, this function prints its results, but it also returns them in an object containing three lists:
  \item{input}{The arguments specified when calling the function}
  \item{intermediate}{Intermediat objects and values}
  \item{output}{The results such as the plot.}
}
\references{
Verboon, P. & Peters, G.-J. Y. (2018) Applying the generalised logistic model in single case designs: modelling treatment-induced shifts. \emph{PsyArXiv} \url{https://doi.org/10.17605/osf.io/ad5eh}}
\author{
Peter Verboon & Gjalt-Jorn Peters (both at the Open University of the Netherlands)

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{genlog}}
}
\examples{
### Load dataset
data(Singh);

### Extract Jason
dat <- Singh[Singh$tier==1, ];

### Conduct piecewise regression analysis
piecewiseRegr(dat,
              timeVar='time',
              yVar='score_physical',
              phaseVar='phase');

### Pretend treatment started between measurements
### 5 and 6
piecewiseRegr(dat,
              timeVar='time',
              yVar='score_physical',
              baselineMeasurements=5);

}
\keyword{ regression }
\keyword{ htest }
\keyword{ hplot }
