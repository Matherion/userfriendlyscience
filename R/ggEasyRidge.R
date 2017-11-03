ggEasyRidge <- function(data, items = NULL,
                        labels = NULL, sortByMean = TRUE,
                        xlab = NULL, ylab = NULL) {
  
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
  
  tmpDf <- data.frame(var = factor(rep(unlist(items),
                                       each=nrow(data)),
                                   levels=items[tmpVarOrder],
                                   labels=labels[tmpVarOrder],
                                   ordered=TRUE),
                      val = unlist(data[, items]));
  
  ### Actual plot
  res <-
    ggplot(data = tmpDf,
           mapping = aes_string(x='val',
                                y='var')) +
    geom_density_ridges(na.rm=TRUE,
                        alpha=.25) +
    theme_minimal() +
    labs(x=xlab,
         y=ylab) +
    theme(axis.ticks.x = element_line());
  
  return(res);
}
