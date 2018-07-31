#' exceptionalScores
#' 
#' A function to detect participants that consistently respond exceptionally.
#' 
#' 
#' @param dat The dataframe containing the variables to inspect, or the vector
#' to inspect (but for vectors, \code{\link{exceptionalScore}} might be more
#' useful).
#' @param items The names of the variables to inspect.
#' @param exception When an item will be considered exceptional, passed on as
#' \code{prob} to \code{\link{exceptionalScore}}.
#' @param totalOnly Whether to return only the number of exceptional scores for
#' each row in the dataframe, or for each inspected item, which values are
#' exceptional.
#' @param append Whether to return the supplied dataframe with the new
#' variable(s) appended (if TRUE), or whether to only return the new
#' variable(s) (if FALSE).
#' @param both Whether to look for both low and high exceptional scores (TRUE)
#' or not (FALSE; see \code{\link{exceptionalScore}}).
#' @param silent Can be used to suppress messages.
#' @param suffix If not returning the total number of exceptional values, for
#' each inspected variable, a new variable is returned indicating which values
#' are exceptional.  The text string is appended to each original variable name
#' to create the new variable names.
#' @param totalVarName If returning only the total number of exceptional
#' values, and appending these to the provided dataset, this text string is
#' used as variable name.
#' @return Either a vector containing the number of exceptional values, a
#' dataset containing, for each inspected variable, which values are
#' exceptional, or the provided dataset where either the total or the
#' exceptional values for each variable are appended.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{exceptionalScore}}
#' @examples
#' 
#' exceptionalScores(mtcars)
#' 
#' @export exceptionalScores
exceptionalScores <- function(dat, items=NULL,
                              exception=.025, totalOnly=TRUE, append=TRUE,
                              both=TRUE, silent=FALSE, suffix = "_isExceptional",
                              totalVarName = "exceptionalScores") {
  
  if (is.data.frame(dat)) {
    if (is.null(items)) {
      items <- names(dat);
      if (!silent) {
        cat("No items specified: extracting all variable names in dataframe.\n");
      }
    }
    exceptionalScores <- dat[, items];
  } else {
    ### Vector provided; store in dataframe.
    exceptionalScores <- data.frame(dat);
    names(exceptionalScores) <- deparse(substitute(dat));
  }
  
  originalCols <- ncol(exceptionalScores);
  exceptionalScores <- data.frame(exceptionalScores[, unlist(lapply(exceptionalScores, is.numeric))]);
  if ((originalCols > ncol(exceptionalScores) & !silent)) {
    cat0("Note: ", originalCols - ncol(exceptionalScores), " variables ",
         "were not numeric and will not be checked for exceptional values.\n");
  }
  
  namesToUse <- paste0(colnames(exceptionalScores), suffix);
  
  exceptionalScores <- apply(exceptionalScores, 2,
                             exceptionalScore, prob = exception, both=both, silent=silent);
  
  colnames(exceptionalScores) <- namesToUse;
  
  if (totalOnly) {
    totalTrues <- rowSums(exceptionalScores, na.rm=TRUE);
    if (append) {
      dat[, totalVarName] <- totalTrues;
      return(dat);
    } else {
      return(totalTrues);
    }    
  } else {
    if (append) {
      return(data.frame(dat,
                        exceptionalScores));
    } else {
      return(exceptionalScores);
    }
  }
  
}
