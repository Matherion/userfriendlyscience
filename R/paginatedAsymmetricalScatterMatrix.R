paginatedAsymmetricalScatterMatrix <- function(dat, x, y,
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
    na.omit(dat[, c(x, y)]);

  ### Convert all variables to numeric vectors, if they weren't already
  res$intermediate$dat <-
    dat <-
    massConvertToNumeric(res$intermediate$dat);
  
  if (length(y) > maxRows) {
    
    res$intermediate$paginationVector <-
      cut(1:length(y),
          breaks = ceiling(length(y) / maxRows),
          labels=FALSE);
    
    res$output$scatterMatrices <- list();
    
    for (currentPage in 1:max(res$intermediate$paginationVector)) {
      
      res$output$scatterMatrices[[currentPage]] <-
        asymmetricalScatterMatrix(dat,
                                  x=x,
                                  y=y[res$intermediate$paginationVector==currentPage],
                                  ...);
    }
    
  } else {
    res$output$scatterMatrices <-
      list(asymmetricalScatterMatrix(dat, x=x, y=y, ...));
  }
  
  ### Set class and return result
  class(res) <- "scatterMatrix.paginated";
  return(res);
  
}
