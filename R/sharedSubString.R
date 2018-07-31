#' sharedSubString
#' 
#' A function to find the longest shared substring in a character vector.
#' 
#' 
#' @param x The character vector to process.
#' @param y Optionally, two single values can be specified. This is probably
#' not useful to end users, but it's used by the function when it calls itself.
#' @return A vector of length one with either the longest substring that occurs
#' in all values of the character vector, or NA if no overlap an be found.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords character
#' @examples
#' 
#'   sharedSubString(c("t0_responseTime", "t1_responseTime", "t2_responseTime"));
#'   ### Returns "_responseTime"
#' 
#' @export sharedSubString
sharedSubString <- function(x, y=NULL) {
  if (!is.null(y)) {
    if (length(x) == 1 && length(y) == 1) {
      if (is.na(x) || is.na(y)) {
        return(NA);
      }
      startPos <- 1;
      while (!grepl(substr(x, startPos, nchar(x)), y)) {
        startPos <- startPos + 1;
      }
      if (startPos < nchar(x)) {
        return(substr(x, startPos, nchar(x)));
      } else {
        endPos <- nchar(x);
        while (!grepl(substr(x, 1, endPos), y)) {
          endPos <- endPos - 1;
        }
        if (endPos > 1) {
          return(substr(x, 1, endPos));
        } else {
          return(NA);
        } 
      }
    } else {
      stop("When specifying both x and y, each must be just one value.");
    }
  } else {
    if (length(x) == 1) {
      return(x);
    } else if (length(x) == 2) {
      return(sharedSubString(x[1], x[2]));
    } else {
      return(sharedSubString(sharedSubString(x[1], x[2]), sharedSubString(x[-1])));
    }
  }
}
