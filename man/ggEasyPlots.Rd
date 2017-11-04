\name{ggEasyPlots}
\alias{ggEasyPlots}
\alias{ggEasyRidge}
\alias{ggEasyBar}
\title{
Convenience functions for ggplots based on multiple variables
}
\description{
These are convenience functions to quickly generate plots for multiple variables, with the variables in the y axis.
}
\usage{
ggEasyRidge(data,
            items = NULL,
            labels = NULL,
            sortByMean = TRUE,
            xlab = NULL,
            ylab = NULL)

ggEasyBar(data,
          items = NULL,
          labels = NULL,
          sortByMean = TRUE,
          xlab = NULL,
          ylab = NULL,
          scale_fill_function =
            scale_fill_viridis(discrete = TRUE,
                               guide = guide_legend(title = NULL,
                                                    nrow=1)),
          fontColor = "white",
          fontSize = 2,
          labelMinPercentage = 1,
          showinLegend = "both")
}
\arguments{
  \item{data}{
  The dataframe containing the variables.
}
  \item{items}{
  The variable names (if not provided, all variables will be used).
}
  \item{labels}{
  Labels can optionally be provided; if they are, these will be used instead of the variable names.
}
  \item{sortByMean}{
  Whether to sort the variables by mean value.
}
  \item{xlab, ylab}{
  The labels for the x and y axes.
}
  \item{scale_fill_function}{
  The function to pass to \code{\link{ggplot}} to provide the colors of the bars.
}
  \item{fontColor, fontSize}{
  The color and size of the font used to display the labels
}
  \item{labelMinPercentage}{
  The minimum percentage that a category must reach before the label is printed (in whole percentages, i.e., on a scale from 0 to 100).
}
  \item{showinLegend}{
  What to show in the legend in addition to the values; nothing ("\code{none}"), the frequencies ("\code{freq}"), the percentages ("\code{perc}"), or both ("\code{both}").
}
}
\value{
A \code{\link{ggplot}} plot is returned.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{geom_ridgeline}}, \code{\link{geom_bar}}
}
\examples{
ggEasyBar(mtcars, c('gear', 'carb'));
ggEasyRidge(mtcars, c('disp', 'hp'));
}
\keyword{ hplot }
