meansDiamondPlot <- function(dat, items = NULL, labels = NULL,
                             decreasing=NULL,
                             conf.level=.95,
                             showData = TRUE, dataAlpha = .1,
                             dataColor = "#444444",
                             diamondColors = NULL,
                             jitterWidth = .5,
                             jitterHeight = .4,
                             returnLayerOnly = FALSE,
                             xlab='Effect size estimate',
                             theme=theme_bw(),
                             ylab=NULL,
                             ...) {

  res <- list();
  res$intermediate <- list();

  if (is.null(items)) items <- names(dat);

  res$intermediate$dat <- varsToDiamondPlotDf(dat, items = items,
                                              labels = labels,
                                              decreasing=decreasing,
                                              conf.level=conf.level);

  ### Get labels from this dataframe, because they may have been sorted
  labels <- res$intermediate$dat$label;

  diamondLayer <- diamondPlot(res$intermediate$dat, ciCols=c('lo', 'mean', 'hi'),
                      yLabels = labels, colorCol=diamondColors,
                      returnLayerOnly = TRUE, ...);

  if (returnLayerOnly) {
    return(diamondLayer);
  }

  plot <- ggplot();

  if (showData) {
    plot <- plot +
      rawDataDiamondLayer(dat, items=items,
                          itemOrder = res$intermediate$dat$rownr);
  }

  plot <- plot + diamondLayer +
    scale_y_continuous(breaks=sort(res$intermediate$dat$rownr),
                       minor_breaks=NULL,
                       labels=res$intermediate$dat$label) +
    theme + ylab(ylab) + xlab(xlab) +
    theme(panel.grid.minor.y=element_blank());

  attr(plot, 'itemOrder') <- res$intermediate$dat$rownr;

  return(plot);
}

