meansDiamondPlot <- function(dat, items = NULL, labels = NULL,
                             decreasing=NULL,
                             conf.level=.95,
                             showData = TRUE, dataAlpha = .1, dataSize=3,
                             dataColor = "#444444",
                             diamondColors = NULL,
                             jitterWidth = .5,
                             jitterHeight = .4,
                             returnLayerOnly = FALSE,
                             xlab='Scores and means',
                             ylab=NULL,
                             theme=theme_bw(),
                             xbreaks = "auto",
                             outputFile = NULL,
                             outputWidth = 10,
                             outputHeight = 10,
                             ggsaveParams = list(units='cm',
                                                 dpi=300,
                                                 type="cairo"),
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
                          itemOrder = res$intermediate$dat$rownr,
                          dataAlpha=dataAlpha,
                          dataColor=dataColor,
                          jitterWidth = jitterWidth,
                          jitterHeight = jitterHeight,
                          size=dataSize);
  }

  plot <- plot + diamondLayer +
    scale_y_continuous(breaks=sort(res$intermediate$dat$rownr),
                       minor_breaks=NULL,
                       labels=res$intermediate$dat$label) +
    theme + ylab(ylab) + xlab(xlab) +
    theme(panel.grid.minor.y=element_blank());

  if (!is.null(xbreaks) &&
      length(xbreaks) == 1 &&
      tolower(xbreaks) == "auto") {
    ### If we only have a few values, set these as xbreaks. If we have
    ### more than 10, don't set any breaks manually
    potentialBreaks <- sort(unique(unlist(dat[, items])));
    if (length(potentialBreaks) <= 10) {
      plot <- plot + scale_x_continuous(breaks=potentialBreaks);
    }
  } else if (is.numeric(xbreaks)) {
    plot <- plot + scale_x_continuous(breaks=xbreaks, labels=xbreaks);
  }
  
  if (!is.null(outputFile)) {
    ggsaveParameters <- c(list(filename = outputFile,
                               plot = plot,
                               width = outputWidth,
                               height = outputHeight),
                          ggsaveParams);
    do.call(ggsave, ggsaveParameters);
  }
  
  attr(plot, 'itemOrder') <- res$intermediate$dat$rownr;

  return(plot);
}

