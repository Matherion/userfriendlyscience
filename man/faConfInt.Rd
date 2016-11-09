\name{faConfInt}
\alias{faConfInt}
\title{
Extract confidence bounds from psych's factor analysis object
}
\description{
This function contains some code from a function in \code{\link{psych}} that's not exported \code{print.psych.fa.ci} but useful nonetheless. It basically takes the outcomes of a factor analysis and extracted the confidence intervals.
}
\usage{
faConfInt(fa)
}
\arguments{
  \item{fa}{
  The object produced by the \code{\link{fa}} function from the \code{\link{psych}} package. It is important that the \code{n.iter} argument of \code{\link{fa}} was set to a realistic number, because otherwise, no confidence intervals will be available.
}
}
\details{
THis function extract confidence interval bounds and combines them with factor loadings using the code from the \code{print.psych.fa.ci} in \code{\link{psych}}.
}
\value{
A list of dataframes, one for each extracted factor, with in each dataframe three variables:
\item{lo}{lower bound of the confidence interval}
\item{est}{point estimate of the factor loading}
\item{hi}{upper bound of the confidence interval}
}
\author{
William Revelle (extracted by Gjalt-Jorn Peters)

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
\dontrun{
### Not run because it takes too long to run to test it,
### and may produce warnings, both because of the bootstrapping
### required to generate the confidence intervals in fa
faConfInt(fa(Thurstone.33, 2, n.iter=100, n.obs=100));
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
