associationsDiamondPlot <- function(dat, covariates, criteria,
                                    labels = NULL,
                                    criteriaLabels = NULL,
                                    decreasing=NULL,
                                    sortBy=NULL,
                                    conf.level=.95,
                                    criteriaColors = brewer.pal(8, 'Set1'),
                                    criterionColor = 'black',
                                    returnLayerOnly = FALSE,
                                    esMetric = 'r',
                                    multiAlpha=.33,
                                    singleAlpha = 1,
                                    showLegend=TRUE,
                                    theme=theme_bw(),
                                    ylab="",
                                    xlab="Effect size estimates",
                                    lineSize = 1,
                                    ...) {

  res <- list(input = as.list(environment()),
              intermediate = list());

  if (is.null(criteriaLabels)) criteriaLabels <- criteria;

  res$intermediate$dat <- lapply(criteria,
                                 associationsToDiamondPlotDf,
                                 dat = dat,
                                 covariates = covariates,
                                 labels = labels,
                                 decreasing=NULL,
                                 conf.level=conf.level,
                                 esMetric = esMetric);
  
  names(res$intermediate$dat) <- criteriaLabels;

  ### Check whether we should sort, and if so, sort. One of these
  ### can be missing, so set default value if one is.
  if (!is.null(sortBy) && is.null(decreasing)) decreasing <- TRUE;
  if (!is.null(decreasing)) {
    if (is.null(sortBy)) sortBy <- names(res$intermediate$dat)[1];
    res$intermediate$sortOrder <-
      order(res$intermediate$dat[[sortBy]][, 'mean'],
            decreasing = decreasing);
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
  
  ### Get diamond layers
  res$intermediate$diamondLayers <- list();
  for (i in 1:length(criteriaLabels)) {
    res$intermediate$diamondLayers[[criteriaLabels[i]]] <-
      diamondPlot(res$intermediate$dat[[criteriaLabels[i]]],
                  ciCols=c('lo', 'es', 'hi'),
                  yLabels = labels,
                  colorCol=ifelse(length(criteria) == 1, criterionColor, criteriaColors[i]),
                  alpha = ifelse(length(criteria) == 1, singleAlpha, multiAlpha),
                  returnLayerOnly = TRUE,
                  size=lineSize, ...);
  }

  plot <- ggplot();

  ### Add diamond layers
  for (i in 1:length(res$intermediate$diamondLayers)) {
    plot <- plot +
      res$intermediate$diamondLayers[[criteriaLabels[i]]];
  }

  plot <- plot +
    scale_y_continuous(breaks=sort(res$intermediate$sortOrder),
                       minor_breaks=NULL,
                       labels=labels) +
    theme + ylab(ylab) + xlab(xlab) +
    theme(panel.grid.minor.y=element_blank());
  
  if (length(criteriaLabels) > 1 & showLegend) {
    ### First have to add a ribbon layer so that we can actually
    ### map the fill aesthetic to something in the plot
    plot <- plot + geom_ribbon(data.frame(colorColumn = factor(criteriaLabels),
                                          x=rep(Inf, length(criteriaLabels)),
                                          ymin=rep(Inf, length(criteriaLabels)),
                                          ymax=rep(Inf, length(criteriaLabels))),
                               mapping=aes_string(x='x', ymin='ymin', ymax='ymax',
                                                  fill='colorColumn'),
                               show.legend=TRUE) +
      ### Override the colors and legend position
      guides(fill=guide_legend(override.aes=list(fill=criteriaColors[1:length(criteriaLabels)]),
                               title=NULL)) +
      theme(legend.position="top");
  }
  
  return(plot);

}

