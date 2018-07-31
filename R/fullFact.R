#' fullFact
#' 
#' This function provides a userfriendly interface to a number of advanced
#' factor analysis functions in the \code{\link{psych}} package.
#' 
#' 
#' @param dat Datafile to analyse; if NULL, a pop-up is provided to select a
#' file.
#' @param items Which variables (items) to factor-analyse. If NULL, all are
#' selected.
#' @param rotate Which rotation to use (see \code{\link{psych}} package).
#' @return The outcomes, which are printed to the screen unless assigned.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{fa.parallel}}, \code{\link{vss}}
#' @keywords univariate
#' @examples
#' 
#' \dontrun{
#'   ### Not run to save processing during package testing
#'   fullFact(attitude);
#' }
#' 
#' @export fullFact
fullFact <- function(dat = NULL, items=NULL, rotate='oblimin') {

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  if (is.null(dat)) {
    dat <- getData();
  }
  
  if (is.null(items)) {
    items <- names(dat);
  }
  
  res$output$parallel <- fa.parallel(dat[, items]);
  res$output$vss <- vss(dat[, items], rotate=rotate);
  
  class(res) <- 'fullFact';
  
  return(res);
  
}

print.fullFact <- function(x, ...) {
  print(x$output);
}

