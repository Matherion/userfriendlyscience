meansDiamondPlot <- function(dat, items = NULL, labels = NULL,
                             decreasing=NULL,
                             conf.level=.95,
                             showData = TRUE, dataAlpha = .1,
                             dataColor = "#444444",
                             diamondColors = NULL,
                             jitterWidth = 1.25,
                             jitterHeight = .5,
                             ...) {

  res <- list();
  res$intermediate <- list();
  
  if (is.null(items)) items <- names(dat);
  if (is.null(labels)) labels <- items;
  
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
  } else {
    ### sortedByMean is used later on to organise the raw data; therefore, this should
    ### reflect the order of the variables on the Y axis regardless of whether they're
    ### reorganised
    sortedByMean <- 1:length(labels);
  }

  plot <- diamondPlot(res$intermediate$dat, ciCols=c('lo', 'mean', 'hi'),
                      yLabels = labels, colorCol=diamondColors, ...); 

  if (showData) {

    rawData <- na.omit(data.frame(value = unlist(dat[, items[sortedByMean]]),
                          labels = rep(1:length(items),
                                       each=nrow(dat))));
    
    plot$layers <- c(geom_jitter(data=rawData,
                                 mapping=aes_string(x='value', y='labels'),
                                 size = 2.5,
                                 color = dataColor,
                                 alpha = dataAlpha,
                                 stroke = 0,
                                 width=jitterWidth,
                                 height=jitterHeight), plot$layers);
  }
  
  return(plot);
}
                                
