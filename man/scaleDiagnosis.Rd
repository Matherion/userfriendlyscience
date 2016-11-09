\name{scaleDiagnosis}
\alias{scaleDiagnosis}
\title{
scaleDiagnosis
}
\description{
scaleDiagnosis provides a number of diagnostics for a scale (an aggregative measure consisting of several items).
}
\usage{
scaleDiagnosis(dat=NULL, items=NULL, plotSize=180, sizeMultiplier = 1,
               axisLabels = "none", scaleReliability.ci=FALSE, conf.level=.95,
               powerHist=TRUE, ...) 
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
  \item{scaleReliability.ci}{
  TRUE or FALSE: whether to compute confidence intervals for Cronbach's Alpha and Omega (uses bootstrapping function in MBESS, takes a while).
}
  \item{conf.level}{
  Confidence of confidence intervals for reliability estimates (if requested with scaleReliability.ci).
}
  \item{powerHist}{
  Whether to use the default ggpairs histogram on the diagonal of the scattermatrix, or whether to use the powerHist version.
}
  \item{...}{
  Additional arguments are passed on to powerHist.
}
}
\details{

Function to generate an object with several useful statistics and a plot to assess how the elements (usually items) in a scale relate to each other, such as Cronbach's Alpha, omega, the Greatest Lower Bound, a factor analysis, and a correlation matrix.

}
\value{

An object with the input and several output variables. Most notably:
  \item{scaleReliability}{The results of scaleReliability.}
  \item{pca}{A Principal Components Analysis}
  \item{fa}{A Factor Analysis}
  \item{describe}{Decriptive statistics about the items}
  \item{scatterMatrix}{A scattermatrix with histograms on the diagonal and correlation coefficients in the upper right half.}
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}

\examples{
### Note: the 'not run' is simply because running takes a lot of time,
###       but these examples are all safe to run!
\dontrun{
### This will prompt the user to select an SPSS file
scaleDiagnosis();

### Generate a datafile to use
exampleData <- data.frame(item1=rnorm(100));
exampleData$item2 <- exampleData$item1+rnorm(100);
exampleData$item3 <- exampleData$item1+rnorm(100);
exampleData$item4 <- exampleData$item2+rnorm(100);
exampleData$item5 <- exampleData$item2+rnorm(100);

### Use a selection of two variables
scaleDiagnosis(dat=exampleData, items=c('item2', 'item4'));

### Use all items
scaleDiagnosis(dat=exampleData);
}
}
\keyword{ utilities }
\keyword{ univar }