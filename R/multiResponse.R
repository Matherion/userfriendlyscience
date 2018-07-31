### For the analysis of multiple response questions, use this function:
### https://stackoverflow.com/questions/9265003/analysis-of-multiple-response


#' Generate a table for multiple response questions
#' 
#' The \code{multiResponse} function mimics the behavior of the table produced
#' by SPSS for multiple response questions.
#' 
#' 
#' @param data Dataframe containing the variables to display.
#' @param items,regex Arguments \code{items} and \code{regex} can be used to
#' specify which variables to process. \code{items} should contain the variable
#' (column) names (or indices), and \code{regex} should contain a regular
#' expression used to match to the column names of the dataframe. If none is
#' provided, all variables in the dataframe are processed.
#' @param endorsedOption Which value represents the endorsed option (note that
#' producing this kind of table requires dichotomous items, where each variable
#' is either endorsed or not endorsed, so this is also a way to treat other
#' variables as dichotomour).
#' @return A dataframe with columns \code{Option}, \code{Frequency},
#' \code{Percentage}, and \code{Percentage of (X) cases}, where X is the number
#' of cases.
#' @author Ananda Mahto; implemented in this package (and tweaked a bit) by
#' Gjalt-Jorn Peters.
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @references This function is based on the excellent and extensive Stack
#' Exchange answer by Ananda Mahto at
#' https://stackoverflow.com/questions/9265003/analysis-of-multiple-response.
#' @keywords utilities
#' @examples
#' 
#' multiResponse(mtcars, c('vs', 'am'));
#' 
#' @export multiResponse
multiResponse = function(data, items=NULL, regex = NULL, endorsedOption = 1) {
  if (is.null(regex) && is.null(items)) {
    items <- names(data);
  } else if (is.null(items)) {
    items <- grep(regex, names(data), value=TRUE)
  }
  
  if (!all(items %in% names(data))) {
    stop("You specified items that do not exist in the data you provided (specifically, ",
         vecTxtQ(items[!items %in% names(data)]), ").");
  }
  
  data = data[, items];
  nrOfEndorsements = sum(data == endorsedOption, na.rm=TRUE);
  endorsementsPerItem = colSums(data == endorsedOption, na.rm=TRUE);
  ### Number of participants; first look for missing values in each
  ### row, then only count participants with at least one valid
  ### value
  nrOfCases = sum(!apply(apply(data, 1, is.na), 2, all));
  totals = as.numeric(c(endorsementsPerItem, nrOfEndorsements));
  res <- data.frame(c(names(endorsementsPerItem), "Total"),
                    totals,
                    (totals/nrOfEndorsements)*100,
                    (totals/nrOfCases)*100);
  names(res) <- c("Option", "Frequency", "Percentage of responses", paste0("Percentage of (", nrOfCases, ") cases"));
  return(res);  
}
