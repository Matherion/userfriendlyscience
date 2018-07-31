#' averageFishersZs
#' 
#' Takes pairs of Fisher's z's and the accompanying n's (sample sizes) and
#' returns their average.
#' 
#' 
#' @param zs The values of Fisher's z.
#' @param ns The sample sizes (ns).
#' @return The average of the Fisher's z values.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{averagePearsonRs}}
#' @examples
#' 
#' averageFishersZs(c(1.1, 5.4), c(10, 30));
#' 
#' @export averageFishersZs
averageFishersZs <- function(zs, ns) {
  if (length(zs) != length(ns)) {
    stop("Vector 'zs' (current length: ", length(zs),
         ") and vector 'ns' (current length: ", length(ns),
         ") must be the same length!");
  }
  return( sum((ns - 3)* zs) / (sum(ns) - 3 * length(ns)));
}
