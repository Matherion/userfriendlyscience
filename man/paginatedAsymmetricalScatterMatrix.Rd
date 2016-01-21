\name{paginatedAsymmetricalScatterMatrix}
\alias{paginatedAsymmetricalScatterMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
paginatedAsymmetricalScatterMatrix
}
\description{
A function that generates a series of asymmetricalScatterMatrices, so that they
can be printed or included in PDFs.
}
\usage{
paginatedAsymmetricalScatterMatrix(dat, cols, rows, maxRows = 5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
  The dataframe containing the variables specified in \code{cols} and \code{rows}.
}
  \item{cols}{
  The names of the variables to use for the columns.
}
  \item{rows}{
  The names of the variables to use for the rows.
}
  \item{maxRows}{
  The maximum number of rows on one 'page' (i.e. in one \code{\link{asymmetricalScatterMatrix}}).
  
}
  \item{\dots}{
  Extra arguments to pass on to each \code{\link{asymmetricalScatterMatrix}} call.
}
}
\value{
An object containing the asymmetricalScatterMatrices in a list:
\item{input}{Input values.}
\item{intermediate}{Some values/objects generated in the process.}
\item{output}{A list containing the object 'scatterMatrices', which is a list of the generated scatterMatrices.}
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{asymmetricalScatterMatrix}}
}
\examples{
\dontrun{
### (Not run by default because it's quite timeconsuming.)
tmp <- paginatedAsymmetricalScatterMatrix(infert, cols=c("parity"),
                                          rows=c("induced", "case",
                                                 "spontaneous", "age",
                                                 "pooled.stratum"),
                                          maxRows = 3,
                                          showCorrelations="top-right");
tmp$output$scatterMatrices[[1]];
}
}
\keyword{ misc }
