determinantImportance <- function(data, determinants, targets,
                                  conf.level = list(means = .9999,
                                                    associations = .95),
                                  subQuestions = NULL,
                                  leftAnchors = rep("-", length(determinants)),
                                  rightAnchors = rep("+", length(determinants)),
                                  orderBy = NULL,
                                  decreasing = NULL,
                                  generateColors = list(means = c("red", "blue", "green"),
                                                        associations = c("red", "grey", "green")),
                                  fullColorRange = NULL,
                                  associationsAlpha = .5,
                                  returnPlotOnly = TRUE,
                                  drawPlot = TRUE,
                                  theme=theme_bw(),
                                  ...) {
  
  if (!all(c(determinants, targets) %in% names(dat))) {
    stop("Not all variables names you passed in arguments ",
         "'determinants' or 'targets' are in your dataset!");
  }

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());
  
  if (is.null(subQuestions)) subQuestions <- determinants;
  
  ### Extract relevant subdatasets
  res$intermediate$determinantsDat <- data[, determinants];
  res$intermediate$dat <- data[, c(determinants, targets)];
  
  ### For the scores, the max and min need to be determined from the data
  res$intermediate$fullColorRange <-
    range(res$intermediate$determinantsDat, na.rm = TRUE);
  
  ### These will be used to determine the breaks in the plot with
  ### the scores
  res$intermediate$uniqueValues <-
    sort(unique(na.omit(unlist(res$intermediate$determinantsDat))));

  ### If only one of the sorting arguments is set, set the other
  ### one on the basis of the defaults; otherwise, store the
  ### passed arguments for use later on.
  if (!is.null(orderBy) && is.null(decreasing)) {
    res$intermediate$decreasing <- TRUE;
  } else if (is.null(orderBy) && !is.null(decreasing)) {
    res$intermediate$orderBy <- TRUE;
  } else {
    res$intermediate$decreasing <- decreasing;
    res$intermediate$orderBy <- orderBy;
  }

  ### Get confidence intervals (we may re-sort later)
  res$intermediate$meansDat <-
    varsToDiamondPlotDf(dat, items = determinants,
                        decreasing=res$intermediate$decreasing,
                        conf.level=conf.level$means);
  
  ### Get confidence intervals for effect sizes
  res$intermediate$assocDat <- sapply(targets, function(currentTarget) {
      return(associationsToDiamondPlotDf(res$intermediate$dat,
                                         determinants,
                                         currentTarget,
                                         decreasing=res$intermediate$decreasing,
                                         esMetric = 'r'));
    }, simplify=FALSE);

  ### We now have dataframes with all means, effect sizes, and the
  ### corresponding confidence intervals. Now we sort them, if we
  ### need to.
  if (!is.null(res$intermediate$orderBy) &&
      !identical(res$intermediate$orderBy, FALSE)) {
    if (isTRUE(res$intermediate$orderBy)) {
      ### Order by determinants means
      res$intermediate$sortOrder <- attr(res$intermediate$meansDat,
                                         'sortedByMean');
    } else {
      if (!(orderBy %in% targets)) {
        stop("Argument 'orderBy' must be either NULL, TRUE, FALSE, ",
             "or the name of one of the target variables. However, ",
             "you passed '", orderBy, "'.");
      }
      assocDat <- attr(res$intermediate$assocDat[[orderBy]],
                                         'sortedByMean');
    }
  } else {
    res$intermediate$sortOrder <- 1:length(determinants);
  }
  
  res$intermediate$meansDat <-
    res$intermediate$meansDat[res$intermediate$sortOrder, ];
  res$intermediate$assocDat <-
    sapply(res$intermediate$assocDat, function(x) {
      return(x[res$intermediate$sortOrder, ]);
  }, simplify=FALSE);
  
  determinants <- determinants[res$intermediate$sortOrder];

  res$intermediate$biAxisDiamondPlot <-
    biAxisDiamondPlot(dat, items = determinants,
                      subQuestions = subQuestions,
                      leftAnchors = leftAnchors,
                      rightAnchors = rightAnchors,
                      generateColors = generateColors$means,
                      fullColorRange = res$intermediate$fullColorRange,
                      drawPlot = FALSE,
                      returnPlotOnly = FALSE,
                      ...);
  
  res$intermediate$meansPlot <- res$intermediate$biAxisDiamondPlot$output$plot;
  
  # yRange <- ggplot_build(res$intermediate$biAxisDiamondPlot$intermediate$meansPlot);
  # yMajor <- yRange$layout$panel_ranges[[1]]$y.major_source;
  # yRange <- range(yRange$layout$panel_ranges[[1]]$y.range);

  res$intermediate$assocLayers <-
    sapply(names(res$intermediate$assocDat),
           function(currentTarget) {
             return(diamondPlot(res$intermediate$assocDat[[currentTarget]],
                                ciCols=c('lo', 'es', 'hi'), yLabels = subQuestions,
                                generateColors=generateColors$associations,
                                fullColorRange = c(-1, 1),
                                alpha = associationsAlpha,
                                returnLayerOnly = TRUE, ...));
           }, simplify=FALSE);
  
  res$intermediate$assocPlot <- ggplot() +
    res$intermediate$assocLayers +
    theme +
    xlab('Associations') +
    scale_x_continuous(limits=c(-1,1)) +
    scale_y_continuous(limits=yRange, breaks=yMajor) +
    theme(axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank());
  
  # builtAssocPlot <- ggplot_build(res$intermediate$assocPlot);
  # builtAssocPlot$layout$panel_ranges[[1]]$y.range <- yRange;

  res$output$plot <- arrangeGrob(res$intermediate$meansPlot,
                                 ggplot_gtable(builtAssocPlot),
                                 ncol=2,
                                 widths=c(2, 1));
  
  if (drawPlot) {
    grid.newpage();
    grid.draw(res$output$plot);
  }
  
  invisible(ifelseObj(returnPlotOnly, res$output$plot, res));

}
