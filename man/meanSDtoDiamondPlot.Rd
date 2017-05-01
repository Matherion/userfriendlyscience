\name{meanSDtoDiamondPlot}
\alias{meanSDtoDiamondPlot}
\title{
A diamond plot based on means, standard deviations, and sample sizes
}
\description{
This function generates a so-called diamond plot: a plot based on the forest plots that are commonplace in meta-analyses. The underlying idea is that point estimates are uninformative, and it would be better to focus on confidence intervals. The problem of the points with errorbars that are commonly employed is that the focus the audience's attention on the upper and lower bounds, even though those are the least relevant values. Using diamonds remedies this.
}
\usage{
meanSDtoDiamondPlot(dat = NULL, means = 1,
                    sds = 2, ns = 3,
                    labels = NULL,
                    colorCol = NULL,
                    conf.level = 0.95,
                    xlab='Means',
                    outputFile = NULL,
                    outputWidth = 10,
                    outputHeight = 10,
                    ggsaveParams = list(units='cm',
                                        dpi=300,
                                        type="cairo"),
                    ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
  The dataset containing the means, standard deviations, sample sizes, and possible labels and manually specified colors.
}
  \item{means}{
  Either the column in the dataframe containing the means, as numeric or as character index, or a vector of means.
}
  \item{sds}{
  Either the column in the dataframe containing the standard deviations, as numeric or as character index, or a vector of standard deviations.
}
  \item{ns}{
  Either the column in the dataframe containing the sample sizes, as numeric or as character index, or a vector of sample sizes.

}
  \item{labels}{
  Optionally, either the column in the dataframe containing labels, as numeric or as character index, or a vector of labels.
}
  \item{colorCol}{
  Optionally, either the column in the dataframe containing manually specified colours, as numeric or as character index, or a vector of manually specified colours.
}  \item{conf.level}{
  The confidence of the confidence intervals.
}
  \item{xlab}{
  The label for the x axis.
}
  \item{outputFile}{
  A file to which to save the plot.
}
  \item{outputWidth, outputHeight}{
  Width and height of saved plot (specified in centimeters by default, see \code{ggsaveParams}).
}
  \item{ggsaveParams}{
  Parameters to pass to ggsave when saving the plot.
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
\code{\link{meansDiamondPlot}}, \code{\link{diamondPlot}}, \code{\link{factorLoadingDiamondCIplot}}, \code{\link{ggDiamondLayer}}
}
\examples{
tmpDf <- data.frame(means = c(1, 2, 3),
                    sds = c(1.5, 3, 5),
                    ns = c(2, 4, 10),
                    labels = c('first', 'second', 'third'),
                    color = c('purple', 'grey', 'orange'));
                    
### A simple diamond plot
meanSDtoDiamondPlot(tmpDf);

### A simple diamond plot with labels
meanSDtoDiamondPlot(tmpDf, labels=4);

### When specifying column names, specify column
### names for all columns
meanSDtoDiamondPlot(tmpDf, means='means',
                    sds='sds', ns='ns',
                    labels='labels');

### A diamond plot using the specified colours
meanSDtoDiamondPlot(tmpDf, labels=4, colorCol=5);

### A diamond plot using automatically generated colours
### using a gradient
meanSDtoDiamondPlot(tmpDf,
                    generateColors=c('green', 'red'));

### A diamond plot using automatically generated colours
### using a gradient, specifying the minimum and maximum
### possible values that can be attained
meanSDtoDiamondPlot(tmpDf,
                    generateColors=c('red', 'yellow', 'blue'),
                    fullColorRange=c(0, 5));
}
\keyword{ hplot }
