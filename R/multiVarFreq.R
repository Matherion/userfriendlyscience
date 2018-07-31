#' Generate a table collapsing frequencies of multiple variables
#' 
#' This function can be used to efficiently combine the frequencies of
#' variables with the same possible values. The frequencies are collapsed into
#' a table with the variable names as row names and the possible values as
#' column (variable) names.
#' 
#' 
#' @param data The dataframe containing the variables.
#' @param items The variable names.
#' @param labels Labels can be provided which will be set as row names when
#' provided.
#' @param sortByMean Whether to sort the rows by mean value for each variable
#' (only sensible if the possible values are numeric).
#' @return The resulting dataframe, but with class 'multiVarFreq' prepended to
#' allow pretty printing.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{table}}, \code{\link{freq}}
#' @keywords utilities
#' @examples
#' 
#' multiVarFreq(mtcars, c('gear', 'carb'));
#' 
#' @export multiVarFreq
multiVarFreq <- function(data,
                         items = NULL,
                         labels = NULL,
                         sortByMean = TRUE) {

  if (is.null(items)) {
    items <- names(data);
  }
  
  if (!all(items %in% names(data))) {
    stop("You specified items that do not exist in the data you provided (specifically, ",
         vecTxtQ(items[!items %in% names(data)]), ").");
  }
  
  if (sortByMean && length(items) > 1) {
    tmpVarOrder <- order(colMeans(data[, items],
                                  na.rm=TRUE),
                         decreasing=FALSE);
  } else {
    tmpVarOrder <- 1:length(items);
  }
  
  if (is.null(labels)) {
    labels <- items;
  }
  
  res <- do.call(rbind.fill,
                 lapply(data[, items],
                        function(x)
                          return(as.data.frame(t(as.matrix(table(x)))))
                 ));
  
  rownames(res) <- labels;
  
  res <- res[tmpVarOrder, ];
  
  if (all(grepl('\\d+', colnames(res)))) {
    res <- res[, order(as.numeric(colnames(res)))];
  }
  
  class(res) <- c('multiVarFreq', class(res));
  
  return(res);
  
}

print.multiVarFreq <- function(x, ...) {
  class(x) <- 'data.frame';
  x <- as.matrix(x);
  print(x, na.print="", ...);
}

pander.multiVarFreq <- function(x, ...) {
  class(x) <- 'data.frame';
  cat("\n\n");
  pander(x, missing = "");
  cat("\n\n");
}
