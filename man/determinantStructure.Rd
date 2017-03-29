\name{determinantStructure}
\alias{determinantStructure}
\alias{determinantVar}
\alias{subdeterminants}
\alias{subdeterminantProducts}
\alias{plot.determinantStructure}
\alias{print.determinantStructure}
\title{
Determinant Structure specification
}
\description{
These functions can be used to specify a determinant structure: a hierarchical
structure of determinants that can then be conveniently plotted and analysed,
for example using \code{\link{detStructCIBER}}.

These functions are made to be used together; see the example and the
forthcoming article for more information.
}
\usage{
determinantStructure(name, selection = NULL, ...)

determinantVar(name, selection = NULL, ...)

subdeterminants(name, selection = NULL, ...)

subdeterminantProducts(name, selection = NULL, ...)

\method{print}{determinantStructure}(x, ...)
\method{plot}{determinantStructure}(x, ...)

}
\arguments{
  \item{name}{
  The name of the variable that is specified.
}
  \item{selection}{
  A regular expression to use to select the variables in a dataframe
  that are considered items that together form this variable. For
  \code{determinantStructure}, a list can be provided that also contains
  a named regular expression with the name 'behaviorRegEx', which specifies
  the name of the behavior to which this determinant structure pertains.
}
  \item{x}{
  The \code{determinantStructure} object to print or plot.
}
  \item{\dots}{
  Any additional arguments are other determinant structure building
  functions. These are used to construct the determinant structure 'tree'.
}
}
\details{
This family of functions will be explained more in detail in a forthcoming
paper.

\code{plot} and \code{print} methods plot and print a determinantStructure object.
}
\value{
A determinantStructure object, which is a \code{\link{data.tree}} object.
}
\references{
(Forthcoming)
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{detStructAddVarLabels}}, \code{\link{detStructAddVarNames}},
\code{\link{detStructComputeProducts}}, \code{\link{detStructComputeScales}},
\code{\link{detStructCIBER}}
}
\examples{
determinantStructure('using R',
                     list('using R',
                          behaviorRegEx = 'some RegEx'),
                     determinantVar("Intention",
                                    "another RegEx",
                                    determinantVar("Attitude",
                                                   "third RegEX",
                                                   subdeterminants("Likelihood",
                                                                   "4th RegEx"),
                                                  subdeterminants("Evaluation",
                                                                  "5th RegEx"),
                                                  subdeterminantProducts("attProduct",
                                                                         c("4th RegEx",
                                                                           "5th RegEx"))),
                                    determinantVar("perceivedNorm",
                                                   "6th RegEx",
                                                   subdeterminants("Approval",
                                                                   "7th RegEx"),
                                                   subdeterminants("Motivation to comply",
                                                                   "8th RegEx"),
                                                   subdeterminantProducts("normProduct",
                                                                          c("7th RegEx",
                                                                            "8th RegEx"))),
                                              determinantVar("pbc",
                                                             "9th RegEx",
                                                             subdeterminants("Control beliefs",
                                                                             "10th RegEx"))));
}
\keyword{ misc }
