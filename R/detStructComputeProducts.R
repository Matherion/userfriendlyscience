detStructComputeProducts <- function(determinantStructure,
                                     dat,
                                     append = TRUE) {

  ### Get all variables names of all 'product halves'
  multiplicables <- determinantStructure$Get("varNames", traversal='level',
                                             filterFun=function(x) {
                                               return(x$type == 'subdeterminantProducts');
                                             }, simplify=FALSE);

  if (getOption('ufs.debug', FALSE)) {
    message("Debugging message:\n  Extracted the following products to compute:\n",
            multiplicables);
  }

  ### Remove those that don't occur in both lists
  actualMultiplicables <- lapply(1:length(multiplicables), function(i) {
    varNameVectors <- list(gsub(paste0('(.*)', names(multiplicables[[i]][1]), '(.*)'),
                                "\\1\\2",
                                multiplicables[[i]][[1]]),
                           gsub(paste0('(.*)', names(multiplicables[[i]][2]), '(.*)'),
                                "\\1\\2",
                                multiplicables[[i]][[2]]));
    varNameVectors <- list(sort(multiplicables[[i]][[1]][varNameVectors[[1]]
                                                         %in% varNameVectors[[2]]]),
                           sort(multiplicables[[i]][[2]][varNameVectors[[2]]
                                                         %in% varNameVectors[[1]]]));
    return(varNameVectors);
  });

  ### Copy dataframe, but drop all variables
  resDat <- dat[, FALSE];

  for (currentSetIndex in 1:length(actualMultiplicables)) {
    ### Identifying but of this product
    productIdentifier <- names(multiplicables)[currentSetIndex];
    ### In the variable names, replace the bit specific to
    ### each half of the product with the name of the
    ### product as specified in the determinant structure
    newNames <- gsub(paste0('(.*)', names(multiplicables[[currentSetIndex]][1]), '(.*)'),
                     paste0("\\1",  productIdentifier, "\\2"),
                     actualMultiplicables[[currentSetIndex]][[1]]);

    determinantStructure$Do(function(currentNode) {
      currentNode$productVarNames <- newNames;
    }, filterFun = function(x) return(x$name==productIdentifier));

    ### Loop through the product halves and compute and store the products
    for (i in 1:length(actualMultiplicables[[currentSetIndex]][[1]])) {

      if (!all(c(actualMultiplicables[[currentSetIndex]][[1]][i],
                 actualMultiplicables[[currentSetIndex]][[2]][i]) %in%
               names(dat))) {
        stop(paste0("One or both of these variables that you specified to ",
                    "multiply to obtain determinant structure product terms ",
                    "does not exist: ",
                    actualMultiplicables[[currentSetIndex]][[1]][i],
                    " and ",
                    actualMultiplicables[[currentSetIndex]][[2]][i]), ".");
      }

      resDat[, newNames[i]] <-
        dat[, actualMultiplicables[[currentSetIndex]][[1]][i]] *
        dat[, actualMultiplicables[[currentSetIndex]][[2]][i]];
    }

  }

  ### Return the results
  if (append) {
    return(cbind(dat, resDat));
  } else {
    return(resDat);
  }

}
