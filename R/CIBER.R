CIBER <- function(data, determinants, targets,
                  conf.level = list(means = .9999,
                                    associations = .95),
                  subQuestions = NULL,
                  leftAnchors = rep("Lo", length(determinants)),
                  rightAnchors = rep("Hi", length(determinants)),
                  orderBy = NULL,
                  decreasing = NULL,
                  numberSubQuestions = FALSE,
                  generateColors = list(means = c("red", "blue", "green"),
                                        associations = c("red", "grey", "green")),
                  strokeColors = brewer.pal(9, 'Set1'),
                  titlePrefix = "Means and associations with",
                  titleVarLabels = NULL,
                  titleSuffix = "",
                  fullColorRange = NULL,
                  associationsAlpha = .5,
                  returnPlotOnly = TRUE,
                  drawPlot = TRUE,
                  baseSize = .8,
                  dotSize = 2.5 * baseSize,
                  baseFontSize=10*baseSize,
                  theme=theme_bw(base_size=baseFontSize),
                  xbreaks=NULL,
                  ...) {

  if (!all(c(determinants, targets) %in% names(data))) {
    stop("Not all variables names you passed in arguments ",
         "'determinants' or 'targets' are in the dataset!\n",
         "Specifically, ",
         vecTxtQ(c(determinants, targets)[!(c(determinants, targets) %in% names(data))]),
         " is or are not in the provided dataset.");
  }

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  if (is.null(subQuestions)) subQuestions <- determinants;

  ### Extract relevant subdatasets
  res$intermediate$determinantsDat <- data[, determinants];
  res$intermediate$dat <- data[, c(determinants, targets)];

  res$output$determinantsN <- sum(complete.cases(res$intermediate$determinantsDat));
  res$output$associationsN <- sum(complete.cases(res$intermediate$dat));

  ### For the scores, the max and min need to be determined from the data
  res$intermediate$fullColorRange <- ifelseObj(is.null(fullColorRange),
                                               range(res$intermediate$determinantsDat, na.rm = TRUE),
                                               fullColorRange);

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

  ### Turn 'decreasing' around, because ggplot places the 'first' values
  ### at the bottom and the last ones at the top
  if (!is.null(res$intermediate$decreasing)) {
    decreasing <- res$intermediate$decreasing <- !res$intermediate$decreasing;
  }

  if (is.null(orderBy)) {
    ### Invert order, because ggplot starts from the bottom on the y axis.
    res$intermediate$sortOrder <- rev(1:length(determinants));
  } else if (isTrue(orderBy)) {
    res$intermediate$sortOrder <- order(colMeans(data[, determinants], na.rm=TRUE),
                                        decreasing=res$intermediate$decreasing);
  } else if (orderBy %IN% (targets)) {
    res$intermediate$sortOrder <- sort(associationMatrix(data,
                                                         x=determinants,
                                                         y=orderBy),
                                       decreasing=res$intermediate$decreasing)$intermediate$sorting$order;
  } else {
    stop("In argument 'orderBy' either pass TRUE (to order by ",
         "(sub)determinants), or the name of one of the target ",
         "variables (e.g. determinants such as attitude, motivational ",
         "constructs such as intention, behavioral proxies or ",
         "behavioral measures).");
  }

  ### Get confidence intervals (we may re-sort later)
  res$intermediate$meansDat <-
    varsToDiamondPlotDf(data, items = determinants,
                        conf.level=conf.level$means);

  ### Get confidence intervals for effect sizes
  res$intermediate$assocDat <- sapply(targets, function(currentTarget) {
    return(associationsToDiamondPlotDf(res$intermediate$dat,
                                       determinants,
                                       currentTarget,
                                       esMetric = 'r'));
  }, simplify=FALSE);
  names(res$intermediate$assocDat) <- targets;

  ### Get R squared values
  res$intermediate$Rsq <- lapply(targets, function(currentTarget) {
    return(regr(formula(paste(currentTarget, '~', paste(determinants, collapse=" + "))),
                data=res$intermediate$dat,
                conf.level=conf.level$associations));
  });

  res$intermediate$meansDat <-
    res$intermediate$meansDat[res$intermediate$sortOrder, ];
  res$intermediate$assocDat <-
    sapply(res$intermediate$assocDat, function(x) {
      return(x[res$intermediate$sortOrder, ]);
    }, simplify=FALSE);

  ### Sort determinant names
  determinants <- determinants[res$intermediate$sortOrder];

  sortedSubQuestions <- subQuestions[res$intermediate$sortOrder];

  if (numberSubQuestions) {
    sortedSubQuestions <- paste0(length(sortedSubQuestions):1, ". ", sortedSubQuestions);
  }

  res$intermediate$biAxisDiamondPlot <-
    biAxisDiamondPlot(data, items = determinants,
                      subQuestions = sortedSubQuestions,
                      leftAnchors = leftAnchors[res$intermediate$sortOrder],
                      rightAnchors = rightAnchors[res$intermediate$sortOrder],
                      generateColors = generateColors$means,
                      fullColorRange = res$intermediate$fullColorRange,
                      conf.level = conf.level$means,
                      drawPlot = FALSE,
                      returnPlotOnly = FALSE,
                      dotSize = dotSize,
                      baseFontSize = baseFontSize,
                      theme = theme,
                      jitterHeight = .3,
                      xbreaks=xbreaks,
                      ...);

  res$intermediate$meansPlot <- res$intermediate$biAxisDiamondPlot$output$plot;

  builtMeansPlot <- ggplot_build(res$intermediate$biAxisDiamondPlot$intermediate$meansPlot);
  yMajor <- builtMeansPlot$layout$panel_ranges[[1]]$y.major_source;
  yRange <- range(builtMeansPlot$layout$panel_ranges[[1]]$y.range);

  if (length(targets)==1) {
    strokeColors <- "#000000";
  } else {
    ### brewer.pal(12, 'Set1')
    strokeColors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                      "#FFFF33", "#A65628", "#F781BF", "#999999")[1:length(targets)];
  }
  names(strokeColors) <- targets;

  res$intermediate$assocLayers <-
    sapply(names(res$intermediate$assocDat),
           function(currentTarget) {
             return(diamondPlot(res$intermediate$assocDat[[currentTarget]],
                                ciCols=c('lo', 'es', 'hi'), yLabels = subQuestions[res$intermediate$sortOrder],
                                generateColors=generateColors$associations,
                                fullColorRange = c(-1, 1),
                                alpha = associationsAlpha,
                                lineColor=strokeColors[currentTarget],
                                size=1, theme=theme,
                                returnLayerOnly = TRUE, ...));
           }, simplify=FALSE);

  res$intermediate$assocPlot <- ggplot() +
    res$intermediate$assocLayers +
    theme +
    xlab(paste0(round(100 * conf.level$associations, 2), '% CIs of associations')) +
    scale_x_continuous(limits=c(-1,1)) +
    scale_y_continuous(breaks=yMajor) +
    theme(axis.ticks.y=element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank());

  builtAssocPlot <- ggplot_build(res$intermediate$assocPlot);
  builtAssocPlot$layout$panel_ranges[[1]]$y.range <- yRange;
  builtAssocPlot$layout$panel_ranges[[1]]$y.major <-
    builtMeansPlot$layout$panel_ranges[[1]]$y.major;


  if (is.null(titleVarLabels)) titleVarLabels <- targets;

  titleGrobs <- list(textGrob(label = paste0(titlePrefix, " "),
                              x = unit(0.2, "lines"),
                              y = unit(0.8, "lines"),
                              hjust = 0, vjust = 0));
  currentXpos <- sum(unit(0.2, "lines"), grobWidth(titleGrobs[[1]]));
  newGrob <- textGrob(label = paste0(titleVarLabels[1], " (R\U00B2 = ",
                                     formatCI(res$intermediate$Rsq[[1]]$output$rsq.ci, noZero=TRUE), ")"),
                      x = currentXpos,
                      y = unit(.8, "lines"),
                      hjust = 0, vjust = 0,
                      gp = gpar(col = strokeColors[targets[1]]));
  titleGrobs <- c(titleGrobs, list(newGrob));
  currentXpos <- sum(currentXpos, grobWidth(titleGrobs[[2]]));

  if (length(targets) > 1) {
    for (i in 2:length(targets)) {
      prefixGrob <- textGrob(label = ifelse(i == length(targets), " & ", ", "),
                             x = currentXpos,
                             y = unit(0.8, "lines"),
                             hjust = 0, vjust = 0,
                             gp = gpar(col = "#000000"));
      currentXpos <- sum(currentXpos, grobWidth(prefixGrob));
      newGrob <- textGrob(label = paste0(titleVarLabels[i], " (R\U00B2 = ",
                                         formatCI(res$intermediate$Rsq[[i]]$output$rsq.ci, noZero=TRUE), ")"),
                          x = currentXpos,
                          y = unit(0.8, "lines"),
                          hjust = 0, vjust = 0,
                          gp = gpar(col = strokeColors[targets[i]]));
      currentXpos <- sum(currentXpos, grobWidth(newGrob));
      titleGrobs <- c(titleGrobs, list(prefixGrob, newGrob));
    }
  }
  titleGrobs <- c(titleGrobs, list(textGrob(label = paste0(" ", titleSuffix),
                                            x = currentXpos,
                                            y = unit(0.8, "lines"),
                                            hjust = 0, vjust = 0)));

  titleGrob <- do.call(grobTree, c(list(gp = gpar(fontsize = 1.2*baseFontSize, fontface = "bold")),
                                   titleGrobs));

  res$output$plot <- gtable_add_cols(res$intermediate$meansPlot,
                                     unit(1, "null"));

  res$output$plot <- gtable_add_grob(res$output$plot,
                                     ggplot_gtable(builtAssocPlot),
                                     t=1,
                                     b=length(res$output$plot$heights),
                                     l=length(res$output$plot$widths));

  res$output$plot <- arrangeGrob(res$output$plot,
                                 top = titleGrob,
                                 padding = unit(1.25, "line"));

  ### Default sizes ; first compute in centimeters, then convert to inches
  attr(res$output$plot, 'height') <- baseSize + 1.25 * baseSize * max(length(determinants), 1.5);
  attr(res$output$plot, 'width') <- 21 - 3;
  attr(res$output$plot, 'height') <- attr(res$output$plot, 'height') / 2.54;
  attr(res$output$plot, 'width') <- attr(res$output$plot, 'width') / 2.54;

  if (drawPlot) {
    grid.newpage();
    grid.draw(res$output$plot);
  }

  invisible(ifelseObj(returnPlotOnly, res$output$plot, res));

}
