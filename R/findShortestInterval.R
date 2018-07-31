### Function to find minimum shortest interval in numeric vector


#' Find the shortest interval
#' 
#' This function takes a numeric vector, sorts it, and then finds the shortest
#' interval and returns its length.
#' 
#' 
#' @param x The numeric vector.
#' @return The length of the shortest interval.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utilities
#' @examples
#' 
#' findShortestInterval(c(1, 2, 4, 7, 20, 10, 15));
#' 
#' @export findShortestInterval
findShortestInterval <- function(x) {
  if (!is.numeric(x))
    stop("This function only accepts numeric vectors as input.");
  if (length(x) == 1)
    return(x);
  x <- sort(x);
  i <- 1;
  res <- abs(x[i] - x[i+1]);
  while (i < length(x)) {
    if (res > abs(x[i] - x[i+1])) res <- abs(x[i] - x[i+1]);
    i <- i + 1;
  }
  return(res);
}
