\name{asymmetricalScatterMatrix}
\alias{asymmetricalScatterMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
asymmetricalScatterMatrix
}
\description{
This function generates an asymmetrical \code{\link{scatterMatrix}} with
histograms showing the distribution of each variable.
}
\usage{
asymmetricalScatterMatrix(dat, cols, rows, theme = dlvTheme(),
                          txtHeight = 1, histHeight = 3,
                          scatterWidth = 6, scatterHeight = 6, unit = "cm",
                          dpi = 200, showCorrelations=c('top-left',
                          'top-right', 'bottom-left', 'bottom-right'),
                          correlationSize = 15,
                          correlationColor = "#cadded",
                          pointSize = 1.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
  The dataframe containing the items to show in the \code{\link{scatterMatrix}}.
}
  \item{cols}{
  The variable names of the variables to place on the columns.
}
  \item{rows}{
  The variable names of the variables to place on the rows.
}
  \item{theme}{
  Which ggplot theme to use.
}
  \item{txtHeight, histHeight, scatterWidth, scatterHeight}{
  These numbers are used to determine the space used for displaying the
  scatterplots, histograms, and labels in the final scatterMatrix.
}
  \item{unit}{
  The unit in which txtHeight, histHeight, scatterWidth, and scatterheight
  are provided.
}
  \item{dpi}{
  The DPI of the final plot.
}
  \item{showCorrelations}{
  Where to display correlation coefficients; set to NULL to display no
  correlation coefficients.
}
  \item{correlationSize}{
  The size(s) of the correlation coefficient(s).
}
  \item{correlationColor}{
  The color of the correlation coefficient(s).
}
  \item{pointSize }{
  The size of the points in the scatterplots.
}
}
\value{
A \code{\link{scatterMatrix}}, just not symmetrical.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
asymmetricalScatterMatrix(infert, cols=c("parity", "age"),
                          rows=c("induced", "case", "spontaneous"),
                          showCorrelations="top-right");}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }