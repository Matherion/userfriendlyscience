#' createSigma: convenience function for mvrnorm
#' 
#' This function is made to quickly generate a Sigma matrix of the type
#' required by \code{\link{mvrnorm}}. By specifying the number of variables,
#' the mean correlation, and how much variation there should be in the
#' correlations, it's easy to quickly generate a correlation matrix.
#' 
#' 
#' @param nVar The number of variables in the correlation matrix.
#' @param meanR The average correlation, provided to \code{\link{rnorm}}
#' together with \code{sdR} to generate the correlations.
#' @param sdR The variation in the correlations, provided to
#' \code{\link{rnorm}} together with \code{meanR} to generate the correlations.
#' @param diagonal The value on the diagonal of the returned matrix: will
#' normally be 1.
#' @return A matrix of nVar x nVar.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{mvrnorm}}, \code{\link{rnorm}}, \code{\link{matrix}}
#' @keywords datagen
#' @examples
#' 
#' createSigma(3, .5, .1);
#' 
#' @export createSigma
createSigma <- function(nVar, meanR = .3, sdR = 0, diagonal = 1) {
  Sigma <- matrix(rnorm(n = nVar^2,
                        mean = meanR,
                        sd = sdR),
                  ncol = nVar);
  Sigma[(Sigma < -1) | (Sigma > 1)] <- 1;
  Sigma[upper.tri(Sigma)] <- t(Sigma[lower.tri(Sigma)])
  if (!is.null(diagonal)) {
    diag(Sigma) <- diagonal;
  }
  return(Sigma);
}
