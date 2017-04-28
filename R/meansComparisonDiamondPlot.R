meansComparisonDiamondPlot <- function(dat, items = NULL,
                                       compareBy = NULL,
                                       labels = NULL,
                                       compareByLabels = NULL,
                                       decreasing=NULL,
                                       sortBy=NULL,
                                       conf.level=.95,
                                       showData = TRUE, dataAlpha = .1, dataSize=3,
                                       comparisonColors = brewer.pal(8, 'Set1'),
                                       alpha = .33,
                                       jitterWidth = .5,
                                       jitterHeight = .4,
                                       xlab='Scores and means',
                                       theme=theme_bw(),
                                       ylab=NULL,
                                       showLegend=TRUE,
                                       lineSize=1,
                                       ...) {

  res <- list();
  res$intermediate <- list();

  if (is.null(items)) items <- names(dat)[2:ncol(dat)-1];
  if (is.null(compareBy)) compareBy <- names(dat)[ncol(dat)];

  res$intermediate$rawDat <- split(dat, dat[, compareBy]);

  ### Get diamondPlotDf's, but don't sort anything yet
  res$intermediate$dat <- lapply(res$intermediate$rawDat,
                                 varsToDiamondPlotDf,
                                 items = items,
                                 labels = labels,
                                 decreasing=NULL,
                                 conf.level=conf.level);

  ### Check whether we should sort, and if so, sort. One of these
  ### can be missing, so set default value if one is.
  if (!is.null(sortBy) && is.null(decreasing)) decreasing <- TRUE;
  if (!is.null(decreasing)) {
    if (is.null(sortBy)) sortBy <- names(res$intermediate$rawDat)[1];
    res$intermediate$sortOrder <-
      order(res$intermediate$rawDat[[sortBy]][, 'mean'],
            decreasing = !decreasing); ## Invert because ggplot plots
                                       ## first elements on y axis lowest
    res$intermediate$dat <- lapply(res$intermediate$dat,
                                   function(df, s = res$intermediate$sortOrder) {
                                     return(df[s, ]);
                                   });
  } else {
    res$intermediate$sortOrder <- 1:nrow(res$intermediate$dat[[1]]);
  }

  ### Get labels from one of these dataframes,
  ### because they may have been sorted
  labels <- res$intermediate$dat[[1]]$label;
  if (is.null(compareByLabels)) compareByLabels <- names(res$intermediate$dat);

  ### Get diamond layers
  res$intermediate$diamondLayers <- list();
  for (i in 1:length(res$intermediate$dat)) {
    res$intermediate$diamondLayers[[compareByLabels[i]]] <-
      diamondPlot(res$intermediate$dat[[compareByLabels[i]]],
                  ciCols=c('lo', 'mean', 'hi'),
                  yLabels = labels, colorCol=comparisonColors[i],
                  alpha = alpha,
                  returnLayerOnly = TRUE,
                  size=lineSize, ...);
  }

  plot <- ggplot();

  ### If requested, get data layers and add these to the plot
  if (showData) {
    res$intermediate$dataLayers <- list();
    for (i in 1:length(res$intermediate$dat)) {
      res$intermediate$dataLayers[[compareByLabels[i]]] <-
        rawDataDiamondLayer(res$intermediate$rawDat[[compareByLabels[i]]],
                            items=items,
                            itemOrder = res$intermediate$sortOrder,
                            dataAlpha = dataAlpha,
                            dataColor = comparisonColors[i],
                            jitterWidth = jitterWidth,
                            jitterHeight = jitterHeight,
                            size=dataSize);
      plot <- plot + res$intermediate$dataLayers[[compareByLabels[i]]];
    }
  }

  ### Add diamond layers
  for (i in 1:length(res$intermediate$dat)) {
    plot <- plot +
      res$intermediate$diamondLayers[[compareByLabels[i]]];
  }

  plot <- plot +
    scale_y_continuous(breaks=sort(res$intermediate$sortOrder),
                       minor_breaks=NULL,
                       labels=labels) +
    theme + ylab(ylab) + xlab(xlab) +
    theme(panel.grid.minor.y=element_blank());

  if (showLegend) {
    ### First have to add a ribbon layer so that we can actually
    ### map the fill aesthetic to something in the plot
    plot <- plot + geom_ribbon(data.frame(colorColumn = factor(compareByLabels),
                                         x=rep(Inf, length(compareByLabels)),
                                         ymin=rep(Inf, length(compareByLabels)),
                                         ymax=rep(Inf, length(compareByLabels))),
                              mapping=aes_string(x='x', ymin='ymin', ymax='ymax',
                                                 fill='colorColumn'),
                              show.legend=TRUE) +
      ### Override the colors and legend position
      guides(fill=guide_legend(override.aes=list(fill=comparisonColors[1:length(compareByLabels)]),
                               title=NULL)) +
      theme(legend.position="top");
  }

  attr(plot, 'itemOrder') <- res$intermediate$sortOrder;

  return(plot);
}

