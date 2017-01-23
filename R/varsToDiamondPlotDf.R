varsToDiamondPlotDf <- function(dat, items = NULL, labels = NULL,
                                decreasing=NULL,
                                conf.level=.95) {
  
  if (is.null(items)) items <- names(dat);
  if (is.null(labels)) labels <- items;
  
  resDf <- data.frame(t(sapply(dat[, items], function(x) {
    x <- na.omit(x);
    ci <- meanConfInt(x, conf.level=conf.level)$output$ci;
    return(data.frame(lo = ci[1], mean = mean(x), hi = ci[2]));
  })));
  
  resDf$label <- labels;
  resDf$rownr <- 1:nrow(resDf);
  resDf$constant <- 1;
  
  if (!is.null(decreasing)) {
    ### Invert 'decreasing' because ggplot plots the lowest/first value first (near the origin).
    ### So a decreasing sort would normally result in higher means being displayed LOWER in
    ### the plot, which is counter-intuitive, hence the inversion.
    sortedByMean <- order(unlist(resDf$mean), decreasing=!decreasing);
    resDf <- resDf[sortedByMean, ];
    labels <- labels[sortedByMean];
  } else {
    ### sortedByMean is used later on to organise the raw data; therefore, this should
    ### reflect the order of the variables on the Y axis regardless of whether they're
    ### reorganised
    sortedByMean <- 1:length(labels);
  }
  
  ### Return this vector as attribute to use in meansDiamondPlot
  attr(resDf, 'sortedByMean') <- sortedByMean;
  
  return(resDf);
  
}
