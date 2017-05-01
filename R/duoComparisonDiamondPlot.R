duoComparisonDiamondPlot <- function(dat, items = NULL,
                                     compareBy = NULL,
                                     labels = NULL,
                                     compareByLabels = NULL,
                                     decreasing=NULL,
                                     conf.level=c(.95, .95),
                                     showData = TRUE, dataAlpha = .1, dataSize=3,
                                     comparisonColors = brewer.pal(8, 'Set1'),
                                     associationsColor = 'grey',
                                     alpha = .33,
                                     jitterWidth = .5,
                                     jitterHeight = .4,
                                     xlab=c('Scores and means',
                                            'Effect size estimates'),
                                     ylab=c(NULL, NULL),
                                     theme=theme_bw(),
                                     showLegend=TRUE,
                                     lineSize=1,
                                     drawPlot = TRUE,
                                     xbreaks="auto",
                                     outputFile = NULL,
                                     outputWidth = 10,
                                     outputHeight = 10,
                                     ggsaveParams = list(units='cm',
                                                         dpi=300,
                                                         type="cairo"),
                                     ...) {

  if (length(unique(na.omit(dat[, compareBy]))) != 2) {
    stop("The variable you compare by ('", compareBy,
         "') has to have exactly two levels. ",
         "It has ", length(unique(na.omit(dat[, compareBy]))), ".");
  }

  associationsDf <- associationsToDiamondPlotDf(dat = dat,
                                                covariates = items,
                                                criterion = compareBy,
                                                decreasing=NULL,
                                                esMetric = "d");

  if (is.null(decreasing)) {
    sortOrder <- 1:nrow(associationsDf);
  } else {
    ### Again, for some reason, R returns a list instead of a vector
    ### when slicing one column from this dataframe
    sortOrder <- order(unlist(associationsDf[, "es"]),
                       decreasing = decreasing);
  }

  ### Invert because ggplot plots first elements on y axis lowest
  #sortOrder <- rev(sortOrder);

  plot1 <- meansComparisonDiamondPlot(dat,
                                      items=items[sortOrder],
                                      compareBy=compareBy,
                                      labels=labels,
                                      compareByLabels = compareByLabels,
                                      decreasing=NULL,
                                      sortBy=NULL,
                                      conf.level=conf.level[1],
                                      showData = showData,
                                      dataAlpha = dataAlpha,
                                      dataSize=dataSize,
                                      comparisonColors = comparisonColors,
                                      alpha = alpha,
                                      jitterWidth = jitterWidth,
                                      jitterHeight = jitterHeight,
                                      xlab=xlab[1],
                                      theme=theme,
                                      ylab=ylab[1],
                                      showLegend=showLegend,
                                      lineSize=lineSize,
                                      xbreaks=xbreaks,
                                      ...);

  plot2 <- associationsDiamondPlot(dat,
                                   covariates=items[sortOrder],
                                   criteria=compareBy,
                                   labels = rep("", length(items)),
                                   criteriaLabels = NULL,
                                   decreasing=NULL,
                                   sortBy=NULL,
                                   conf.level=conf.level[2],
                                   criterionColor = associationsColor,
                                   returnLayerOnly = FALSE,
                                   esMetric = "d",
                                   theme=theme_bw(),
                                   ylab="",
                                   xlab=xlab[2],
                                   lineSize = lineSize,
                                   ...)

  builtMeansPlot <- ggplot_build(plot1);
  yMajor <- builtMeansPlot$layout$panel_ranges[[1]]$y.major_source;
  yRange <- range(builtMeansPlot$layout$panel_ranges[[1]]$y.range);

  builtAssocPlot <- ggplot_build(plot2);
  builtAssocPlot$layout$panel_ranges[[1]]$y.range <- yRange;
  builtAssocPlot$layout$panel_ranges[[1]]$y.major <-
    builtMeansPlot$layout$panel_ranges[[1]]$y.major;

  plot1grob <- ggplot_gtable(builtMeansPlot);
  plot2grob <- ggplot_gtable(builtAssocPlot);

  ### Add row in plot2 for the legend; first get row position & height
  legendRow <- plot1grob$layout[plot1grob$layout$name=='guide-box',
                                c("t", "b")];
  legendHeight <- plot1grob$heights[legendRow[['b']]];
  rowBelowLegendHeight <- plot1grob$heights[legendRow[['b']]+1];
  rowAboveLegend <- min(legendRow) - 1;

  plot2grob <- gtable_add_rows(plot2grob,
                               rowBelowLegendHeight,
                               pos = rowAboveLegend);
  plot2grob <- gtable_add_rows(plot2grob,
                               legendHeight,
                               pos = rowAboveLegend);

  plot <- gtable_add_cols(plot1grob,
                          unit(1, "null"));

  plot <- gtable_add_grob(plot,
                          plot2grob,
                          t=1,
                          b=length(plot$heights),
                          l=length(plot$widths));

  if (drawPlot) {
    grid.newpage();
    grid.draw(plot);
  }

  if (!is.null(outputFile)) {
    ggsaveParameters <- c(list(filename = outputFile,
                               plot = plot,
                               width = outputWidth,
                               height = outputHeight),
                          ggsaveParams);
    do.call(ggsave, ggsaveParameters);
  }

  invisible(plot);

}
