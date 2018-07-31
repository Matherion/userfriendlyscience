#' removeExceptionalValues
#' 
#' A function to replace exceptional values with NA. This can be used to
#' quickly remove impossible values, for example, when participants entered
#' their age as 344.
#' 
#' Note that exceptional values may be errors (e.g. participants accidently
#' pressed a key twice, or during data entry, something went wrong), but they
#' may also be indicative of participants who did not seriously participate in
#' the study. Therefore, it is advised to first use
#' \code{\link{exceptionalScores}} to look for patterns where participants
#' enter many exceptional scores.
#' 
#' @param dat The dataframe containing the items to inspect.
#' @param items The items to inspect.
#' @param exception How rare a value must be to be considered exceptional (and
#' replaced by NA).
#' @param silent Can be used to suppress messages.
#' @param stringsAsFactors Whether to convert strings to factors when creating
#' a dataframe from lapply output.
#' @return The dataframe, with exceptional values replaced by NA.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{exceptionalScores}}
#' @keywords utilities
#' @examples
#' 
#' removeExceptionalValues(mtcars, exception=.1);
#' 
#' @export removeExceptionalValues
removeExceptionalValues <- function(dat, items=NULL, exception=.005,
                                    silent=FALSE, stringsAsFactors=FALSE) {
  if (is.data.frame(dat)) {
    if (is.null(items)) {
      items <- names(dat);
      if (!silent) {
        cat("No items specified: extracting all variable names in dataframe.\n");
      }
    }
    return(data.frame(lapply(dat, function(x) {
      if (is.numeric(x)) {
        return(ifelse(exceptionalScore(x, prob = exception), NA, x));
      } else {
        return(x);
      }
    }), stringsAsFactors=stringsAsFactors));
  } else {
    return(ifelse(exceptionalScore(dat, prob = exception), NA, dat));
  }
}
