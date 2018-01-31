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
          showInLegend = "both",
          biAxisLabels = NULL)
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
  \item{showInLegend}{
  What to show in the legend in addition to the values; nothing ("\code{none}"), the frequencies ("\code{freq}"), the percentages ("\code{perc}"), or both ("\code{both}"). This is only used if only one variable is shown in the plot; afterwise, after all, the absolute frequencies and percentages differ for each variable.
}
  \item{biAxisLabels}{
  This can be used to specify labels to use if you want to use labels on both the left and right side. This is mostly useful when plotting single questions or semantic differentials. This must be a list with two character vectors, \code{leftAnchors} and \code{rightAnchors}, which must each have the same length as the number of items specified in \code{items}. See the examples for, well, examples.
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

### When plotting single questions, if you want to show the anchors:
ggEasyBar(mtcars, c('gear'),
          biAxisLabels=list(leftAnchors="Fewer",
                            rightAnchors="More"));

### Or for multiple questions (for e.g. semantic differentials):
ggEasyBar(mtcars, c('gear', 'carb'),
          biAxisLabels=list(leftAnchors=c("Fewer", "Lesser"),
                            rightAnchors=c("More", "Greater")));
}
\keyword{ hplot }
