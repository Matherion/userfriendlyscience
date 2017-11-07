\name{reliability}
\alias{reliability}
\title{
Reliability function similar to the SPSS RELIABILITY command
}
\description{
This function was developed to offer a function that roughly works similar to the SPSS RELIABILITY command.
}
\usage{
reliability(data,
            items = NULL,
            itemDiagnostics = FALSE,
            digits = 2)
}
\arguments{
  \item{data}{
  The dataframe containing the variables (items, questions) of interest.
}
  \item{items}{
  Optionally, the variables (items, questions) of interest. If omitted, all variables (items, questions) in the dataframe will be used.
}
  \item{itemDiagnostics}{
  Whether to also display the item diagnostics (specifically, the corrected item-total correlation, mean and variance excluding each item, and the reliability coefficients excluding each item).
}
  \item{digits}{
  The number of digits to use when displaying the results.
}
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{scaleStructure}}, the excellent \code{\link{psych}} package
}
\examples{
\dontrun{
## (Not run to test because it takes a long time.)

data(testRetestSimData);
reliability(testRetestSimData[, 2:11], itemDiagnostics = TRUE);
}
}
\keyword{ univar }
