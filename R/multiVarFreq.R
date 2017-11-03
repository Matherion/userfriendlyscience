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
                         decreasing=TRUE);
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
  print(as.matrix(x), na.print="", ...);
}

pander.multiVarFreq <- function(x, ...) {
  class(x) <- 'data.frame';
  cat("\n\n");
  pander(x, missing = "");
  cat("\n\n");
}
