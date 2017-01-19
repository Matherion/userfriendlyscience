\name{powerHist}
\alias{powerHist}
\title{
powerHist
}
\description{
powerHist generates a histogram with a density curve and a normal density curve.
}
\usage{
powerHist(vector, histColor = "#0000CC",
          distributionColor = "#0000CC",
          normalColor = "#00CC00", distributionLineSize = 1,
          normalLineSize = 1, histAlpha = 0.25, xLabel = NULL,
          yLabel = NULL, normalCurve = TRUE, distCurve = TRUE,
          breaks = 30, theme = dlvTheme(),
          rug = NULL, jitteredRug = TRUE, rugSides = "b",
          rugAlpha = .2, returnPlotOnly = FALSE)
          
}
\arguments{
  \item{vector}{
  A numeric vector.
}
  \item{histColor}{
  The colour to use for the histogram.
}
  \item{distributionColor}{
  The colour to use for the density curve.
}
  \item{normalColor}{
  The colour to use for the normal curve.
}
  \item{distributionLineSize}{
  The line size to use for the distribution density curve.
}
  \item{normalLineSize}{
  The line size to use for the normal curve.
}
\item{histAlpha}{
  Alpha value ('opaqueness', as in, versus transparency) of the histogram.
}
  \item{xLabel}{
  Label to use on x axis.
}
  \item{yLabel}{
  Label to use on y axis.
}
  \item{normalCurve}{
  Whether to display the normal curve.
  }
  \item{distCurve}{
  Whether to display the curve showing the distribution of the observed data.
  }
  \item{breaks}{
  The number of breaks to use (this is equal to the number of bins minus one,
  or in other words, to the number of bars minus one).
  }
\item{theme}{
  The theme to use.
}
\item{rug}{
  Whether to add a rug (i.e. lines at the bottom that correspond to individual
  datapoints.
}
\item{jitteredRug}{
  Whether to jitter the rug (useful for variables with several datapoints
  sharing the same value.
}
\item{rugSides}{
  This is useful when the histogram will be rotated; for example, this can be
  set to 'r' if the histogram is rotated 270 degrees.
}
\item{rugAlpha}{
  Alpha value to use for the rug. When there is a lot of overlap, this can help
  get an idea of the number of datapoints at 'popular' values.
}
\item{returnPlotOnly}{
  Whether to return the usual \code{powerHist} object that also contains
  all settings and intermediate objects, or whether to only return the
  \code{\link{ggplot}} plot.
}
}
\value{

An object, with the following elements:
  \item{input}{The input when the function was called.}
  \item{intermediate}{The intermediate numbers and distributions.}
  \item{dat}{The dataframe used to generate the plot.}
  \item{plot}{The histogram.}
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}

\examples{

powerHist(mtcars$mpg)

}
\keyword{ utilities }
