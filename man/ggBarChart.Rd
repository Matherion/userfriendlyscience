\name{ggBarChart}
\alias{ggBarChart}
\title{
Bar chart using ggplot
}
\description{
This function provides a simple interface to create a \code{\link{ggplot}} bar chart.
}
\usage{
ggBarChart(vector, plotTheme = theme_bw(), ...)
}
\arguments{
  \item{vector}{
  The vector to display in the bar chart.
}
  \item{plotTheme}{
  The theme to apply.
}
  \item{\dots}{
  And additional arguments are passed to \code{\link{geom_bar}}.
}
}
\value{
A \code{\link{ggplot}} plot is returned.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}\seealso{
\code{\link{geom_bar}}
}
\examples{
ggBarChart(mtcars$cyl);
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
