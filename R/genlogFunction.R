####  Definition Generalized Logistic function (NB "B" is in exp()) with scaling factor


#' Generalized Logistic Function
#' 
#' This is the core function of the generalized logistic analysis used in
#' \code{\link{genlog}}.
#' 
#' For details, see Verboon & Peters (2017).
#' 
#' @param x A numeric vector with measurement moments or indices of measurement
#' moments.
#' @param x0 A single numeric value specifying at which moment the curve is at
#' its midpoint (when \code{v} = 1).
#' @param Ab,At Respectively the lowest and highest possible values of the
#' dependent variable.
#' @param B The growth rate (curve steepness).
#' @param v Um - Peter, wat is 'v' eigenlijk?
#' @author Peter Verboon (Open University of the Netherlands)
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{genlog}}
#' @references Verboon, P. & Peters, G.-J. Y. (2017) Applying the generalised
#' logistic model in SCD to deal with ceiling effects. \emph{PsyArXiv}
#' http://INSERTLINK
#' @keywords utilities
#' @examples
#' 
#' time <- 1:20;
#' yVar <- genlogFunction(1:20, 10, 1, 7, 1, 1);
#' plot(time, yVar, type='l', xlab='time', ylab='y');
#' 
#' @export genlogFunction
genlogFunction <- function(x, x0, Ab, At, B, v) {
  return(Ab + ((At - Ab)/ (1 + exp(-B*(x-x0)))**(1/v)));
}
