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
ggEasyBar(data,
          items = NULL,
          labels = NULL,
          sortByMean = TRUE,
          xlab = NULL,
          ylab = NULL,
          scale_fill_function =
            scale_fill_viridis(discrete = TRUE,
                               guide = guide_legend(title = NULL,
                                                    nrow=1)))
          
ggEasyRidge(data,
            items = NULL,
            labels = NULL,
            sortByMean = TRUE,
            xlab = NULL,
            ylab = NULL)
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
