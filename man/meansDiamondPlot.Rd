\name{meansDiamondPlot}
\alias{meansDiamondPlot}
\title{
Diamond plots
}
\description{
This function generates a so-called diamond plot: a plot based on the forest plots that are commonplace in meta-analyses. The underlying idea is that point estimates are uninformative, and it would be better to focus on confidence intervals. The problem of the points with errorbars that are commonly employed is that the focus the audience's attention on the upper and lower bounds, even though those are the least relevant values. Using diamonds remedies this.
}
\usage{
meansDiamondPlot(dat, items = NULL,
                 labels = NULL,
                 decreasing = NULL,
                 conf.level = 0.95,
                 showData = TRUE,
                 dataAlpha = 0.1,
                 dataColor = "#444444",
                 diamondColors = NULL,
                 ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
  The dataframe containing the variables (\code{items}) to show in the diamond plot.
}
  \item{items}{
  Optionally, the names (or numeric indices) of the  variables (items) to show in the diamond plot. If NULL, all columns (variables, items) will be used.
}
  \item{labels}{
  A character vector of labels to use instead of column names from the dataframe.
}
  \item{decreasing}{
  Whether to sort the variables (rows) in the diamond plot decreasing (TRUE), increasing (FALSE), or not at all (NULL).
}
  \item{conf.level}{
  The confidence of the confidence intervals.
}
  \item{showData}{
  Whether to show the raw data or not.
}
  \item{dataAlpha}{
  This determines the alpha (transparency) of the data points. Note that argument \code{alpha} can be used to set the alpha of the diamonds; this is eventually passed on to \code{\link{ggDiamondLayer}}.
}
  \item{dataColor}{
  The color of the data points.
}
  \item{diamondColors}{
  A vector of the same length as there are rows in the dataframe,
  to manually specify colors for the diamonds.
}
  \item{\dots}{
  Additional arguments are passed to \code{\link{diamondPlot}} and eventually to \code{\link{ggDiamondLayer}}. This can be used to, for example, specify two or more colors to use to generate a gradient (using \code{generateColors} and maybe \code{fullColorRange}).
}
}
\value{
A \code{\link{ggplot}} plot with a \code{\link{ggDiamondLayer}} is returned.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{diamondPlot}}, \code{\link{meanSDtoDiamondPlot}}, \code{\link{factorLoadingDiamondCIplot}}, \code{\link{ggDiamondLayer}}
}
\examples{
tmpDf <- data.frame(item1 = rnorm(50, 1.6, 1),
                    item2 = rnorm(50, 2.6, 2),
                    item3 = rnorm(50, 4.1, 3));
                    
### A simple diamond plot
meansDiamondPlot(tmpDf);

### A diamond plot with manually
### specified labels and colors
meansDiamondPlot(tmpDf,
                 labels=c('First',
                          'Second',
                          'Third'),
                  diamondColors=c('blue', 'magenta', 'yellow'));

### Using a gradient for the colors
meansDiamondPlot(tmpDf,
                 labels=c('First',
                          'Second',
                          'Third'),
                 generateColors = c("magenta", "cyan"),
                 fullColorRange = c(1,5));
}
\keyword{ hplot }
