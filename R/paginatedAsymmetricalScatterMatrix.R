#' paginatedAsymmetricalScatterMatrix
#' 
#' A function that generates a series of asymmetricalScatterMatrices, so that
#' they can be printed or included in PDFs.
#' 
#' 
#' @param dat The dataframe containing the variables specified in \code{cols}
#' and \code{rows}.
#' @param cols The names of the variables to use for the columns.
#' @param rows The names of the variables to use for the rows.
#' @param maxRows The maximum number of rows on one 'page' (i.e. in one
#' \code{\link{asymmetricalScatterMatrix}}).
#' @param \dots Extra arguments to pass on to each
#' \code{\link{asymmetricalScatterMatrix}} call.
#' @return An object containing the asymmetricalScatterMatrices in a list:
#' \item{input}{Input values.} \item{intermediate}{Some values/objects
#' generated in the process.} \item{output}{A list containing the object
#' 'scatterMatrices', which is a list of the generated scatterMatrices.}
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{asymmetricalScatterMatrix}}
#' @keywords misc
#' @examples
#' 
#' \dontrun{
#' ### (Not run by default because it's quite timeconsuming.)
#' tmp <- paginatedAsymmetricalScatterMatrix(infert, cols=c("parity"),
#'                                           rows=c("induced", "case",
#'                                                  "spontaneous", "age",
#'                                                  "pooled.stratum"),
#'                                           maxRows = 3,
#'                                           showCorrelations="top-right");
#' tmp$output$scatterMatrices[[1]];
#' }
#' 
#' @export paginatedAsymmetricalScatterMatrix
paginatedAsymmetricalScatterMatrix <- function(dat, cols, rows,
                                               maxRows = 5,
                                               ...) {
  
  ### Generate object with 3 sub-objects to store input,
  ### intermediate results, and output
  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());
  
  ### Extract dataframe and select only complete cases
  res$intermediate$dat <-
    dat <-
    na.omit(dat[, c(cols, rows)]);

  ### Convert all variables to numeric vectors, if they weren't already
  res$intermediate$dat <-
    dat <-
    massConvertToNumeric(res$intermediate$dat);
  
  if (length(rows) > maxRows) {
    
    res$intermediate$paginationVector <-
      cut(1:length(rows),
          breaks = ceiling(length(rows) / maxRows),
          labels=FALSE);
    
    res$output$scatterMatrices <- list();
    
    for (currentPage in 1:max(res$intermediate$paginationVector)) {
      
      res$output$scatterMatrices[[currentPage]] <-
        asymmetricalScatterMatrix(dat,
                                  cols=cols,
                                  rows=rows[res$intermediate$paginationVector==currentPage],
                                  ...);
    }
    
  } else {
    res$output$scatterMatrices <-
      list(asymmetricalScatterMatrix(dat, cols=cols, rows=rows, ...));
  }
  
  ### Set class and return result
  class(res) <- "scatterMatrix.paginated";
  return(res);
  
}
