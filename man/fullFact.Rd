\name{fullFact}
\alias{fullFact}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
fullFact
}
\description{
This function provides a userfriendly interface to a number of advanced factor
analysis functions in the \code{\link{psych}} package.
}
\usage{
fullFact(dat = NULL, items = NULL, rotate = "oblimin")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
  Datafile to analyse; if NULL, a pop-up is provided to select a file.
}
  \item{items}{
  Which variables (items) to factor-analyse. If NULL, all are selected.
}
  \item{rotate}{
  Which rotation to use (see \code{\link{psych}} package).
}
}
\value{
The outcomes, which are printed to the screen unless assigned.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{fa.parallel}}, \code{\link{vss}}
}
\examples{
\dontrun{
  ### Not run to save processing during package testing
  fullFact(attitude);
}
}
\keyword{ univariate }
