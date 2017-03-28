\name{pwr.randomizationSuccess}
\alias{pwr.randomizationSuccess}
\title{
Compute sample size required for successful randomization
}
\description{
This function computes the sample size required to make randomization
succeed in a specified proportion of the studies with a two-cell design.
'Success' is defined as the two groups differing at most with a specified
effect size on any of a given number or nuisance variables.
}
\usage{
pwr.randomizationSuccess(dNonequivalence = 0.2,
                         pRandomizationSuccess = 0.95,
                         nNuisanceVars = 100)
}
\arguments{
  \item{dNonequivalence}{
  The maximum difference between the two groups that is deemed
  acceptable.
}
  \item{pRandomizationSuccess}{
  The desired probability that the randomization procedure succeeded in
  generating two equivalent groups (i.e. differing at most with
  \code{dNonequivalence}).
}
  \item{nNuisanceVars}{
  The number of nuisance variables that the researchers assumes exists.
}
}
\details{
  For more details, see Peters & Gruijters (2017).
}
\value{
  The required sample size; the function is vectorized, so returns either
  a vector of length one, a vector of length > 1, a matrix, or an array.
}
\references{
Peters, G. J.-Y. & Gruijters, S. Why your experiments fail: sample sizes required for randomization to generate equivalent groups as a partial solution to the replication crisis (2017). http://dx.doi.org/
}
\author{
Gjalt-Jorn Peters (Open University of the Netherlands, Greater Good, Maastricht University) & Stefan Gruijters (Maastricht University).

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dNonequivalence = 0.2, pRandomizationSuccess = 0.95, 
    nNuisanceVars = 100) 
{
    res <- sapply(dNonequivalence, function(dNonequival) {
        return(sapply(pRandomizationSuccess, function(rRandSuccess, 
            dNonequiv = dNonequival) {
            return(sapply(nNuisanceVars, function(nNuisance, 
                pSuccess = rRandSuccess, dNoneq = dNonequiv) {
                n <- 10
                if (pdMild(dNoneq, n)^nNuisance > pSuccess) {
                  return(n)
                } else {
                  while (pdMild(dNoneq, n)^nNuisance < pSuccess) {
                    n <- n + 100
                  }
                  n <- n - 100
                  while (pdMild(dNoneq, n)^nNuisance < pSuccess) {
                    n <- n + 10
                  }
                  n <- n - 10
                  while (pdMild(dNoneq, n)^nNuisance < pSuccess) {
                    n <- n + 1
                  }
                  return(n)
                }
            }))
        }))
    })
    res <- array(unclass(res), dim = c(length(nNuisanceVars), 
        length(pRandomizationSuccess), length(dNonequivalence)), 
        dimnames = list(paste("Nuisance var:", nNuisanceVars), 
            paste("Equival. prob:", pRandomizationSuccess), paste("Nonequival. at: d=", 
                dNonequivalence)))
    if (sum(dim(res) == 1) > 1) {
        return(as.vector(res))
    }
    else if (sum(dim(res) == 1) == 1) {
        dims <- dim(res)
        dimNms <- dimnames(res)
        dimNms <- dimNms[dims > 1]
        dims <- dims[dims > 1]
        res <- matrix(res, ncol = dims[1], dimnames = dimNms)
        return(res)
    }
    else {
        return(res)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
