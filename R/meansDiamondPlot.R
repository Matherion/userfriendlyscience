meansDiamondPlot <- function(dat, items = NULL, labels = NULL,
                             decreasing=NULL,
                             conf.level=.95,
                             showData = TRUE,
                             ...) {

  res <- list();
  res$intermediate <- list();
  
  if (is.null(items)) items <- names(dat);
  if(is.null(labels)) labels <- items;
  
  res$intermediate$dat <- data.frame(t(sapply(dat[, items], function(x) {
    x <- na.omit(x);
    ci <- meanConfInt(x, conf.level=conf.level)$output$ci;
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

  if (showData) {

    rawData <- na.omit(data.frame(value = unlist(dat[, items[sortedByMean]]),
                          labels = rep(1:length(items),
                                       each=nrow(dat))));
    
    plot$layers <- c(geom_jitter(data=rawData,
                                 mapping=aes(x=value, y=labels),
                                 size = 2.5,
                                 color = "#444444",
                                 alpha = .1,
                                 stroke = 0,
                                 width=1.25,
                                 height=.25), plot$layers);
  }
  
  return(plot);
}
                                
