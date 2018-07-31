#' averagePearsonRs
#' 
#' Takes pairs of Pearson r's (correlation coefficients) and the accompanying
#' n's (sample sizes) and returns their average.
#' 
#' 
#' @param rs The correlation coefficients.
#' @param ns The sample sizes.
#' @param FishersZ Whether to compute the average through Fisher's z (only
#' method implemented as of the writing of this document).
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{averageFishersZs}}, \code{\link{convert.r.to.fisherz}}
#' @keywords univariate
#' @examples
#' 
#' averagePearsonRs(c(.3, .4, .6), c(70, 80, 50));
#' 
#' @export averagePearsonRs
averagePearsonRs <- function(rs, ns, FishersZ=TRUE) {
  if (FishersZ) {
    return(convert.fisherz.to.r(averageFishersZs(convert.r.to.fisherz(rs), ns)));
  } else {
    warning("Sorry, Alexander's method (1990, Bulletin of the Psychonomic Society) not yet implemented!");
  }
}
