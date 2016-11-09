\name{ggBoxplot}
\alias{ggBoxplot}
\title{
Box plot using ggplot
}
\description{
This function provides a simple interface to create a \code{\link{ggplot}} box plot, organising different boxplots by levels of a factor is desired, and showing row numbers of outliers.
}
\usage{
ggBoxplot(dat, y = NULL, x = NULL,
          labelOutliers = TRUE,
          outlierColor = "red",
          theme = theme_bw(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
  Either a vector of values (to display in the box plot) or a dataframe containing variables to display in the box plot.
}
  \item{y}{
  If \code{dat} is a dataframe, this is the name of the variable to make the box plot of.
}
  \item{x}{
  If \code{dat} is a dataframe, this is the name of the variable (normally a factor) to place on the X axis. Separate box plots will be generate for each level of this variable.
}
  \item{labelOutliers}{
  Whether or not to label outliers.
}
  \item{outlierColor}{
  If labeling outliers, this is the color to use.
}
  \item{theme}{
  The theme to use for the box plot.
}
  \item{\dots}{
  Any additional arguments will be passed to \code{\link{geom_boxplot}}.
}
}
\details{
This function is based on JasonAizkalns' answer to a question on Stack Exchange (Cross Validated; see \url{http://stackoverflow.com/questions/33524669/labeling-outliers-of-boxplots-in-r}).
}
\value{
A \code{\link{ggplot}} plot is returned.
}
\author{
Jason Aizkalns; implemented in this package (and tweaked a bit) by Gjalt-Jorn Peters.

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{geom_boxplot}}
}
\examples{
### A box plot for miles per gallon in the mtcars dataset:
ggBoxplot(mtcars$mpg);

### And separate for each level of 'cyl' (number of cylinder):
ggBoxplot(mtcars, y='mpg', x='cyl');
}
\keyword{ hplot }
