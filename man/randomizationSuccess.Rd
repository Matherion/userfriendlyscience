\name{randomizationSuccess}
\alias{pwr.randomizationSuccess}
\alias{prob.randomizationSuccess}
\title{
Computations for successful randomization
}
\description{
\code{prob.randomizationSuccess} computes the probability that two groups are
equivalent given a specific sample size, number of nuisance variables,
and definition of 'equivalence' (in terms of the Cohen's d expressing the
maximum acceptable difference between the groups on any of the nuisance
variables).

\code{pwr.randomizationSuccess} computes the sample size required to make randomization
succeed in a specified proportion of the studies with a two-cell design.
'Success' is defined as the two groups differing at most with a specified
effect size on any of a given number or nuisance variables.
}
\usage{
prob.randomizationSuccess(n = 1000,
                          dNonequivalence = .2,
                          nNuisanceVars = 100)
pwr.randomizationSuccess(dNonequivalence = 0.2,
                         pRandomizationSuccess = 0.95,
                         nNuisanceVars = 100)
}
\arguments{
  \item{n}{
  The sample size.
}
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
  For \code{prob.randomizationSuccess}, the probability that the two groups
  are equivalent. The function is vectorized, so returns either a vector
  of length one, a vector of length > 1, a matrix, or an array.
  
  For \code{pwr.randomizationSuccess}, the required sample size. The function is
  vectorized, so returns either a vector of length one, a vector of
  length > 1, a matrix, or an array.
}
\references{
Peters, G. J.-Y. & Gruijters, S. Why your experiments fail: sample sizes required for randomization to generate equivalent groups as a partial solution to the replication crisis (2017). https://doi.org/
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{dCohensd}}
}
\examples{
### To be on the safe side: sample size required to
### obtain 95\% likelihood of success when assuming
### 100 nuisance variables exist.
pwr.randomizationSuccess(dNonequivalence = 0.2,
                         pRandomizationSuccess = 0.95,
                         nNuisanceVars = 100);

### Living on the edge:
pwr.randomizationSuccess(dNonequivalence = 0.2,
                         pRandomizationSuccess = 0.60,
                         nNuisanceVars = 10);

### For those with quite liberal ideas of 'equivalence':
pwr.randomizationSuccess(dNonequivalence = 0.5,
                         pRandomizationSuccess = 0.95,
                         nNuisanceVars = 100);

### And these results can be checked with
### prob.randomizationSuccess:
prob.randomizationSuccess(1212, .2, 100);
prob.randomizationSuccess(386, .2, 10);
prob.randomizationSuccess(198, .5, 100);

### Or in one go:
prob.randomizationSuccess(n=c(198, 386, 1212), c(.2, .5), c(10, 100));

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
