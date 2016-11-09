\name{extractVarName}
\alias{extractVarName}
\title{
Extract variable names
}
\description{
Functions often get passed variables from within dataframes or other lists. However, printing these names with all their dollar signs isn't very userfriendly. This function simply uses a regular expression to extract the actual name.
}
\usage{
extractVarName(x)
}
\arguments{
  \item{x}{
  A character vector of one or more variable names.
}
}
\value{
The actual variables name, with all containing objectes stripped off.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
extractVarName('mtcars$mpg');
}
\keyword{ utils }
