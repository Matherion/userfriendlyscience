meansDiamondPlot <- function(dat, items = NULL, labels = NULL,
                             decreasing=NULL, ...) {
  
  res <- list();
  res$intermediate <- list();
  
  if (is.null(items)) items <- names(dat);
  if(is.null(labels)) labels <- items;
  
  res$intermediate$dat <- data.frame(t(sapply(dat[, items], function(x) {
    x <- na.omit(x);
    ci <- meanConfInt(x)$output$ci;
    return(data.frame(lo = ci[1], mean = mean(x), hi = ci[2]));
  })));
  
  res$intermediate$dat$label <- labels;
  res$intermediate$dat$rownr <- 1:nrow(res$intermediate$dat);
  res$intermediate$dat$constant <- 1;

  if (!is.null(decreasing)) {
    ### Invert 'decreasing' because ggplot plots the lowest/first value first (near the origin).
    ### So a decreasing sort would normally result in higher means being displayed LOWER in
    ### the plot, which is counter-intuitive, hence the inversion.
    sortedByMean <- order(unlist(res$intermediate$dat$mean), decreasing=!decreasing);
    res$intermediate$dat <- res$intermediate$dat[sortedByMean, ];
    labels <- labels[sortedByMean];
  }
  
  plot <- diamondPlot(res$intermediate$dat, ciCols=c('lo', 'mean', 'hi'),
                      yLabels = labels, ...); 
  
  return(plot);
}
                                
