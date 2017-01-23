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
  
  res$intermediate$dat <- varsToDiamondPlotDf(dat, items = items,
                                              labels = labels,
                                              decreasing=NULL,
                                              conf.level=conf.level);
  
  if (is.null(labels)) labels <- res$intermediate$dat$labels;
  sortedByMean <- attr(res$intermediate$dat, 'sortedByMean');

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
                                
