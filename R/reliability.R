#' Reliability function similar to the SPSS RELIABILITY command
#' 
#' This function was developed to offer a function that roughly works similar
#' to the SPSS RELIABILITY command.
#' 
#' 
#' @param data The dataframe containing the variables (items, questions) of
#' interest.
#' @param items Optionally, the variables (items, questions) of interest. If
#' omitted, all variables (items, questions) in the dataframe will be used.
#' @param itemDiagnostics Whether to also display the item diagnostics
#' (specifically, the corrected item-total correlation, mean and variance
#' excluding each item, and the reliability coefficients excluding each item).
#' @param digits The number of digits to use when displaying the results.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{scaleStructure}}, the excellent \code{\link{psych}}
#' package
#' @keywords univar
#' @examples
#' 
#' \dontrun{
#' ## (Not run to test because it takes a long time.)
#' 
#' data(testRetestSimData);
#' reliability(testRetestSimData[, 2:11], itemDiagnostics = TRUE);
#' }
#' 
#' @export reliability
reliability <- function(data,
                        items = NULL,
                        itemDiagnostics = FALSE,
                        digits = 2) {
  if (!is.data.frame(data)) {
    stop("Argument 'data' must be a dataframe. You provided '",
         deparse(substitute(data)), "' which has class ",
         vecTxtQ(class(data)), ".");
  }
  if (is.null(items)) {
    items <- names(data);
  } else {
    if (!all(items %in% names(data))) {
      stop("Not all variables you provided in argument ",
           "'items' are present in dataframe '",
           deparse(substitute(data)), "'. Specifically, ",
           vecTxtQ(items[!(items %in% names(data))]),
           ifelse(length(items[!(items %in% names(data))]) == 1, " is", " are"),
           " not.");
    }
  }
  print(scaleStructure(data, items=items, silent=TRUE));
  if (itemDiagnostics) {
    
    ### Get subset of data, also to organise the variables in the
    ### order of 'items' so that we can use seq_along to address/omit
    ### items
    dat <- data[, items];
    
    fullScale <- rowMeans(dat, na.rm=TRUE);
    cat0("\nScale mean:     ", round(mean(fullScale, na.rm=TRUE), digits), "\n");
    cat0("Scale variance: ", round(var(fullScale, na.rm=TRUE), digits), "\n");

    cat0("\nCorrected item-total correlation and scale properties without each item:\n");
    
    itemDiagnosticsDf <- do.call(rbind, lapply(seq_along(items), function(itemToOmit) {
      item <- dat[, itemToOmit];
      incompleteDat <- dat[, -itemToOmit];
      restOfTheScale <- rowMeans(incompleteDat, na.rm = TRUE);
      meanSansItem <- mean(restOfTheScale, na.rm=TRUE);
      varSansItem <- var(restOfTheScale, na.rm=TRUE);
      itemTotalCor <- cor(item, restOfTheScale);
      scaleStructureSansItem <- scaleStructure(incompleteDat,
                                               ci=FALSE,
                                               poly=FALSE,
                                               omega.psych=FALSE,
                                               samples=1)$output;
      if (!('omega' %in% names(scaleStructureSansItem))) {
        res <- data.frame(itemTotalCor,
                          meanSansItem,
                          varSansItem,
                          scaleStructureSansItem$cronbach.alpha);
        names(res) <- c('Item-total r',
                        'Scale mean',
                        'Scale var.',
                        'Coeff. Alpha');
        
      } else {
        res <- data.frame(itemTotalCor,
                          meanSansItem,
                          varSansItem,
                          scaleStructureSansItem$cronbach.alpha,
                          scaleStructureSansItem$omega,
                          scaleStructureSansItem$glb,
                          scaleStructureSansItem$coefficientH);
        names(res) <- c('Item-total r',
                        'Scale mean',
                        'Scale var.',
                        'Coeff. Alpha',
                        'Omega',
                        'GLB',
                        'Coeff. H');
      }
      return(res);
    }));
    
    row.names(itemDiagnosticsDf) <- items;
    
    cat("\n");
    
    print(itemDiagnosticsDf, digits=digits);

  }
}
