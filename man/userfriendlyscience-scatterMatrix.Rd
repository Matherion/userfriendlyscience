\name{scatterMatrix}
\alias{scatterMatrix}
\title{
scatterMatrix
}
\description{
scatterMatrix produced a matrix with jittered scatterplots, histograms, and correlation coefficients.
}
\usage{
scatterMatrix(dat, items=NULL, plotSize=180, sizeMultiplier = 1,
              axisLabels = "none", powerHist=TRUE, ...)
}
\arguments{
  \item{dat}{
  A dataframe containing the items in the scale. All variables in this
  dataframe will be used if items is NULL.
}
  \item{items}{
  If not NULL, this should be a character vector with the names of the
  variables in the dataframe that represent items in the scale.
}
  \item{plotSize}{
  Size of the final plot in millimeters.
}
  \item{sizeMultiplier}{
  Allows more flexible control over the size of the plot elements
}
  \item{axisLabels}{
  Passed to ggpairs function to set axisLabels.
}
  \item{powerHist}{
  Whether to use the default ggpairs histogram on the diagonal of the scattermatrix, or whether to use the powerHist version.
}
  \item{...}{
  Additional arguments are passed on to powerHist.
}
}
\value{

An object with the input and several output variables. Most notably:
  \item{output$scatterMatrix}{A scattermatrix with histograms on the diagonal and correlation coefficients in the upper right half.}
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
### Note: the 'not run' is simply because running takes a lot of time,
###       but these examples are all safe to run!
\dontrun{

### Generate a datafile to use
exampleData <- data.frame(item1=rnorm(100));
exampleData$item2 <- exampleData$item1+rnorm(100);
exampleData$item3 <- exampleData$item1+rnorm(100);
exampleData$item4 <- exampleData$item2+rnorm(100);
exampleData$item5 <- exampleData$item2+rnorm(100);

### Use all items
scatterMatrix(dat=exampleData);
}
}
\keyword{ utilities }
\keyword{ univar }