biAxisDiamondPlot <- function(dat, items = NULL,
                              leftAnchors=NULL, rightAnchors=NULL,
                              subQuestions=NULL,
                              decreasing = NULL, conf.level = 0.95,
                              showData = TRUE, dataAlpha = 0.1, dataColor = "#444444",
                              diamondColors = NULL, jitterWidth = .45, jitterHeight = .45,
                              xBreaks=NULL, xLabels=NA,
                              xAxisLab = paste0('Scores and ', round(100 * conf.level, 2), "% CIs"),
                              drawPlot = TRUE, returnPlotOnly=TRUE,
                              baseSize = 1,
                              dotSize = baseSize,
                              baseFontSize=10*baseSize,
                              theme=theme_bw(base_size=baseFontSize),
                              ...) {

  if (length(leftAnchors) != length(rightAnchors)) {
    stop("Arguments 'leftAnchors' (", vecTxtQ(leftAnchors),
         ") and 'rightAnchors' (", vecTxtQ(rightAnchors),
         ") have different lengths (", length(leftAnchors),
         " and ", length(rightAnchors), ", respectively), so I'm aborting.");
  }

  if (length(leftAnchors) != length(subQuestions)) {
    stop("Arguments 'leftAnchors' (", vecTxtQ(leftAnchors),
         "), 'rightAnchors' (", vecTxtQ(rightAnchors),
         "), and 'subQuestions' (", vecTxtQ(subQuestions),
         ") have different lengths (", length(leftAnchors),
         ", ", length(rightAnchors), ", and ", length(subQuestions),
         ", respectively), so I'm aborting.");
  }

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  if (is.null(items)) items <- names(dat);
  if (is.null(leftAnchors)) leftAnchors <- rep("lo", length(items));
  if (is.null(rightAnchors)) rightAnchors <- rep("hi", length(items));
  if (is.null(subQuestions)) subQuestions <- items;

  ### Generate plot
  plot <- meansDiamondPlot(dat=dat, items = items, decreasing = decreasing,
                           conf.level = conf.level, showData = showData, dataAlpha = dataAlpha,
                           dataColor = dataColor, diamondColors = diamondColors,
                           jitterWidth = jitterWidth, jitterHeight = jitterHeight,
                           xlab = xAxisLab, theme=theme, size=dotSize,
                           ...);

  ### Extract order of the items after sorting
  res$intermediate$itemOrder <- itemOrder <- attr(plot, 'itemOrder');

  ### Add scale with anchors at both sides (ordered using itemOrder)
  suppressMessages(plot <- plot + scale_y_continuous(breaks=1:length(leftAnchors),
                       labels=leftAnchors[itemOrder],
                       sec.axis = sec_axis(~., breaks=1:length(rightAnchors),
                                           labels=rightAnchors[itemOrder])));

  if (is.null(xBreaks)) {
    xBreaks <- sort(unique(na.omit(unlist(dat[, items]))));
  }

  if (length(xBreaks) > 1) {
    if (!is.na(xLabels[1])) {
      plot <- plot + scale_x_continuous(breaks=xBreaks, labels=xLabels);
    } else {
      plot <- plot + scale_x_continuous(breaks=xBreaks);
    }
  }

  res$intermediate$meansPlot <- plot;

  ### Generate a plot that we'll only use to extract the subquestions
  subQuestionLabelplot <-
    meansDiamondPlot(dat=dat, items = items, decreasing = decreasing,
                     conf.level = conf.level, showData = showData, dataAlpha = dataAlpha,
                     dataColor = dataColor, diamondColors = diamondColors,
                     jitterWidth = jitterWidth, jitterHeight = jitterHeight,
                     xlab = xAxisLab, theme=theme, size=dotSize,
                     ...);

  suppressMessages(subQuestionLabelplot <- subQuestionLabelplot +
    scale_y_continuous(breaks=1:length(leftAnchors),
                       labels=leftAnchors[itemOrder],
                       sec.axis = sec_axis(~., breaks=1:length(rightAnchors), labels=subQuestions[itemOrder])) +
    theme(axis.text.y = element_text(size=rel(1.25), color="black"),
          axis.ticks.y = element_blank()));

  res$intermediate$subQuestionLabelplot <- subQuestionLabelplot;

  ### http://stackoverflow.com/questions/12409960/ggplot2-annotate-outside-of-plot
  ### http://stackoverflow.com/questions/17492230/how-to-place-grobs-with-annotation-custom-at-precise-areas-of-the-plot-region/17493256#17493256
  ### https://github.com/baptiste/gridextra/wiki/gtable
  ### http://stackoverflow.com/questions/37984000/how-to-manage-the-t-b-l-r-coordinates-of-gtable-to-plot-the-secondary-y-axi

  ### Extract grob with axis labels of secondary axis (at the right-hand side),
  ### which are the subquestions
  subQuestionLabelplotAsGrob <- ggplotGrob(subQuestionLabelplot);
  subQuestionPanel <- gtable_filter(subQuestionLabelplotAsGrob, "axis-r");

  ### Compute how wide this grob is based on the width of the
  ### widest element, and express this in inches
  maxSubQuestionWidth <- max(unlist(lapply(lapply(unlist(strsplit(subQuestions, "\n")),
                                                  unit, x=1, units="strwidth"), convertUnit, "inches")));

  ### Convert the real plot to a gtable
  plotAsGrob <- ggplotGrob(plot);

  index <- which(subQuestionLabelplotAsGrob$layout$name == "axis-r");
  subQuestionWidth <-
    subQuestionLabelplotAsGrob$widths[subQuestionLabelplotAsGrob$layout[index, ]$l];

  ### Add a column to the left, with the width of the subquestion grob
  fullPlot <- gtable_add_cols(plotAsGrob, subQuestionWidth, pos=0);

  ### Get the layout information of the panel to locate the subquestion
  ### grob at the right height (i.e. in the right row)
  index <- plotAsGrob$layout[plotAsGrob$layout$name == "panel", ];

  ### Add the subquestion grob to the plot
  fullPlot <- gtable_add_grob(fullPlot,
                              subQuestionPanel,
                              t=index$t, l=1, b=index$b, r=1,
                              name = "subquestions");

  res$output$plot <- fullPlot;

  if (drawPlot == TRUE) {
    grid.newpage();
    grid.draw(fullPlot);
  }

  invisible(ifelseObj(returnPlotOnly, res$output$plot, res));

}
