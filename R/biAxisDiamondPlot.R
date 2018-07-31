#' Diamondplot with two Y axes
#' 
#' This is basically a \code{\link{meansDiamondPlot}}, but extended to allow
#' specifying subquestions and anchors at the left and right side. This is
#' convenient for psychological questionnaires when the anchors or dimensions
#' were different from item to item. This function is used to function the left
#' panel of the \code{\link{CIBER}} plot.
#' 
#' This is a diamondplot that can be used for items/questions where the anchors
#' of the response scales could be different for every item. For the rest, it
#' is very similar to \code{\link{meansDiamondPlot}}.
#' 
#' @param dat The dataframe containing the variables.
#' @param items The variables to include.
#' @param leftAnchors The anchors to display on the left side of the left hand
#' panel. If the items were measured with one variable each, this can be used
#' to show the anchors that were used for the respective scales. Must have the
#' same length as \code{items}.
#' @param rightAnchors The anchors to display on the left side of the left hand
#' panel. If the items were measured with one variable each, this can be used
#' to show the anchors that were used for the respective scales. Must have the
#' same length as \code{items}.
#' @param subQuestions The subquestions used to measure each item. This can
#' also be used to provide pretty names for the variables if the items were not
#' measured by one question each. Must have the same length as \code{items}.
#' @param decreasing Whether to sort the items. Specify \code{NULL} to not sort
#' at all, \code{TRUE} to sort in descending order, and \code{FALSE} to sort in
#' ascending order.
#' @param conf.level The confidence levels for the confidence intervals.
#' @param showData Whether to show the individual datapoints.
#' @param dataAlpha The alpha level (transparency) of the individual
#' datapoints.  Value between 0 and 1, where 0 signifies complete transparency
#' (i.e. invisibility) and 1 signifies complete 'opaqueness'.
#' @param dataColor The color to use for the individual datapoints.
#' @param diamondColors The colours to use for the diamonds. If NULL, the
#' \code{generateColors} argument can be used which will then be passed to
#' \code{\link{diamondPlot}}.
#' @param jitterWidth How much to jitter the individual datapoints
#' horizontally.
#' @param jitterHeight How much to jitter the individual datapoints vertically.
#' @param xbreaks Which breaks to use on the X axis (can be useful to override
#' \code{\link{ggplot}}'s defaults).
#' @param xLabels Which labels to use for those breaks (can be useful to
#' override \code{\link{ggplot}}'s defaults; especially useful in combination
#' with \code{xBreaks} of course).
#' @param xAxisLab Axis label for the X axis.
#' @param drawPlot Whether to draw the plot, or only return it.
#' @param returnPlotOnly Whether to return the entire object that is generated
#' (including all intermediate objects) or only the plot.
#' @param baseSize This can be used to efficiently change the size of most plot
#' elements.
#' @param dotSize This is the size of the points used to show the individual
#' data points in the left hand plot.
#' @param baseFontSize This can be used to set the font size separately from
#' the \code{baseSize}.
#' @param theme This is the theme that is used for the plots.
#' @param outputFile A file to which to save the plot.
#' @param outputWidth,outputHeight Width and height of saved plot (specified in
#' centimeters by default, see \code{ggsaveParams}).
#' @param ggsaveParams Parameters to pass to ggsave when saving the plot.
#' @param \dots These arguments are passed on to \code{\link{diamondPlot}}.
#' @return Either just a plot (a \code{\link{gtable}} object) or an object with
#' all produced objects and that plot.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{CIBER}}, \code{\link{associationsDiamondPlot}}
#' @keywords hplot
#' @examples
#' 
#' biAxisDiamondPlot(dat=mtcars,
#'                   items=c('cyl', 'wt'),
#'                   subQuestions=c('cylinders', 'weight'),
#'                   leftAnchors=c('few', 'light'),
#'                   rightAnchors=c('many', 'heavy'),
#'                   xbreaks=0:8);
#' 
#' @export biAxisDiamondPlot
biAxisDiamondPlot <- function(dat, items = NULL,
                              leftAnchors=NULL, rightAnchors=NULL,
                              subQuestions=NULL,
                              decreasing = NULL, conf.level = 0.95,
                              showData = TRUE, dataAlpha = 0.1, dataColor = "#444444",
                              diamondColors = NULL, jitterWidth = .45, jitterHeight = .45,
                              xbreaks=NULL, xLabels=NA,
                              xAxisLab = paste0('Scores and ', round(100 * conf.level, 2), "% CIs"),
                              drawPlot = TRUE, returnPlotOnly=TRUE,
                              baseSize = 1,
                              dotSize = baseSize,
                              baseFontSize=10*baseSize,
                              theme=theme_bw(base_size=baseFontSize),
                              outputFile = NULL,
                              outputWidth = 10,
                              outputHeight = 10,
                              ggsaveParams = list(units='cm',
                                                  dpi=300,
                                                  type="cairo"),
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
                           xlab = xAxisLab, theme=theme, dataSize=dotSize,
                           ...);

  ### Extract order of the items after sorting
  res$intermediate$itemOrder <- itemOrder <- attr(plot, 'itemOrder');

  ### Add scale with anchors at both sides (ordered using itemOrder)
  suppressMessages(plot <- plot + scale_y_continuous(breaks=1:length(leftAnchors),
                       labels=leftAnchors[itemOrder],
                       sec.axis = sec_axis(~., breaks=1:length(rightAnchors),
                                           labels=rightAnchors[itemOrder])));

  if (is.null(xbreaks)) {
    xbreaks <- sort(unique(na.omit(unlist(dat[, items]))));
    if (length(xbreaks) > 10) {
      xbreaks <- pretty(xbreaks, n=7);
    }
  }

  if (length(xbreaks) > 1) {
    if (!is.na(xLabels[1])) {
      if (length(xbreaks) == length(xLabels)) {
        suppressMessages(plot <- plot + scale_x_continuous(breaks=xbreaks, labels=xLabels));
      } else {
        suppressMessages(plot <- plot + scale_x_continuous(breaks=xbreaks));
        warning("Ignoring 'xLabels' (", vecTxtQ(xLabels),
                "): it has a different length from 'xbreaks' (",
                vecTxtQ(xbreaks), ").");
      }
    } else {
      suppressMessages(plot <- plot + scale_x_continuous(breaks=xbreaks));
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

  if (!is.null(outputFile)) {
    ggsaveParameters <- c(list(filename = outputFile,
                               plot = fullPlot,
                               width = outputWidth,
                               height = outputHeight),
                          ggsaveParams);
    do.call(ggsave, ggsaveParameters);
  }

  if (drawPlot == TRUE) {
    grid.newpage();
    grid.draw(fullPlot);
  }

  invisible(ifelseObj(returnPlotOnly, res$output$plot, res));

}
