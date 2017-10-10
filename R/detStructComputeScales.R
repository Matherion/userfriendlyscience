detStructComputeScales <- function(determinantStructure,
                                   dat,
                                   append = TRUE,
                                   separator = "_") {
  
  if (!("determinantStructure" %in% class(determinantStructure))) {
    stop("The first argument must be a determint structure object!");
  }
  
  if (!("data.frame" %in% class(dat))) {
    stop("The first argument must be a dataframe!");
  }
  
  ### Get behavior regex
  ### Get all behaviorRegExes that are set (should only be one)
  behaviorRegEx <- determinantStructure$Get('behaviorRegEx',
                                            traversal='level',
                                            filterFun=function(x) return(!is.null(x$behaviorRegEx)));
  
  ### Remove any duplicates and select the first one in case there are more
  behaviorRegEx <- unique(behaviorRegEx);
  
  if (length(behaviorRegEx) > 1) {
    warning("The determinant structure you specified has more than one behavior regular expression defined. Only using the first one, '",
            behaviorRegEx[1], "'.");
  }
  
  behaviorRegEx <- behaviorRegEx[1];
  
  ### Get all variables names of all 'product halves'
  scalables <- determinantStructure$Get("varNames", traversal='level',
                                        filterFun=function(x) {
                                          return(x$type == 'determinantVar');
                                        }, simplify=FALSE);

  ### Remove superfluous level in between
  scalables <- lapply(scalables, unlist);

  ### Add behavior before variable names
  names(scalables) <- paste0(behaviorRegEx, separator, names(scalables));
  
  ### Add new variable names to determinant structure
  determinantStructure$Set(scaleVarName = names(scalables),
                           filterFun=function(x) {
                             return(x$type == 'determinantVar');
                           });

  dat <- makeScales(dat, scalables);

  if (append) {
    return(dat);
  } else {
    return(dat[, names(scalables)]);
  }
  
}
