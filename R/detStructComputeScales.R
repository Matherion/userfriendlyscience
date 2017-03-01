detStructComputeScales <- function(determinantStructure,
                                   dat,
                                   append = TRUE) {
  
  ### Get all variables names of all 'product halves'
  scalables <- determinantStructure$Get("varNames", traversal='level',
                                        filterFun=function(x) {
                                          return(x$type == 'determinantVar');
                                        }, simplify=FALSE);
  
  ### Remove superfluous level in between
  scalables <- lapply(scalables, unlist);
  
  dat <- makeScales(dat, scalables);
  
  if (append) {
    return(dat);
  } else {
    return(dat[, names(scalables)]);
  }
  
}
