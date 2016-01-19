\name{powerHist}
\alias{powerHist}
\title{
powerHist
}
\description{
powerHist generates a histogram with a density curve and a normal density curve.
}
\usage{
powerHist(vector, distributionColor = "#2222CC",
          normalColor = "#00CC00", distributionLineSize = 2,
          normalLineSize = 1, xLabel = NULL,
          yLabel = NULL, density=FALSE)
}
\arguments{
  \item{vector}{
  A numeric vector.
}
  \item{distributionColor}{
  The colour to use for the distribution (the histogram and the density curve).
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
  \item{xLabel}{
  Label to use on x axis.
}
  \item{yLabel}{
  Label to use on y axis.
}
  \item{density}{
  This argument is not in use yet.
}
}
\value{

An object, with the following elements:
  \item{input}{The input when the function was called.}
  \item{intermediate}{The intermediate numbers and distributions.}
  \item{dat}{The dataframe used to generate the plot.}
  \item{plot}{The histogram.}
}
\examples{

powerHist(mtcars$mpg)

}
\keyword{ utilities }