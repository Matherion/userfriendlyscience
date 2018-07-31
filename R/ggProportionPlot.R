#' Sample distribution based plotting of proportions
#' 
#' This function visualises percentages, but avoids a clear cut for the sample
#' point estimate, instead using the confidence (as in confidence interval) to
#' create a gradient. This effectively hinders drawing conclusions on the basis
#' of point estimates, thereby urging a level of caution that is consistent
#' with what the data allows.
#' 
#' This function used \code{\link{confIntProp}} to compute confidence intervals
#' for proportions at different levels of confidence. The confidence interval
#' bounds at those levels of confidence are then used to draw rectangles with
#' colors in a gradient that corresponds to the confidence level.
#' 
#' Note that percentually, the gradient may not look continuous because at the
#' borders between lighter and darker rectangles, the shade of the lighter
#' rectangle is perceived as even lighter than it is, and the shade of the
#' darker rectangle is perceived as even darker. This makes it seem as if each
#' rectange is coloured with a gradient in the opposite direction.
#' 
#' @param dat The dataframe containing the items (variables), or a vector.
#' @param items The names of the items (variables). If none are specified, all
#' variables in the dataframe are used.
#' @param loCategory The value of the low category (usually 0). If not
#' provided, the minimum value is used.
#' @param hiCategory The value of the high category (usually 1). If not
#' provided, the maximum value is used.
#' @param subQuestions The labels to use for the variables (for example,
#' different questions). The variable names are used if these aren't provided.
#' @param leftAnchors The labels for the low categories. The values are used if
#' these aren't provided.
#' @param rightAnchors The labels for the high categories. The values are used
#' if these aren't provided.
#' @param compareHiToLo Whether to compare the percentage of low category
#' values to the total of the low category values and the high category values,
#' or whether to ignore the high category values and compute the percentage of
#' low category values relative to all cases. This can be useful when a
#' variable has more than two values, and you only want to know/plot the
#' percentage relative to the total number of cases.
#' @param showDiamonds Whether to add diamonds to illustrate the confidence
#' intervals.
#' @param diamonds.conf.level The confidence level of the diamonds' confidence
#' intervals.
#' @param diamonds.alpha The alpha channel (i.e. transparency, or rather
#' 'obliqueness') of the diamonds.
#' @param na.rm Whether to remove missing values.
#' @param barHeight The height of the bars, or rather, half the height. Use .5
#' to completely fill the space.
#' @param conf.steps The number of steps to use to generate the confidence
#' levels for the proportion.
#' @param scale_color,scale_fill A vector with two values (valid colors), that
#' are used for the colors (stroke) and fill for the gradient; both vectors
#' should normally be the same, but if you feel adventurous, you can play
#' around with the number of \code{conf.steps} and this. If you specify only
#' one color, no gradient is used but a single color (i.e. specifying the same
#' single color for both \code{scale_color} and \code{scale_fill} simply draws
#' bars of that color).
#' @param linetype The \code{\link{linetype}} to use (0 = blank, 1 = solid, 2 =
#' dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash).
#' @param theme The theme to use.
#' @param returnPlotOnly Whether to only return the \code{\link{ggplot2}} plot
#' or the full object including intermediate values and objects.
#' @return A \code{\link{ggplot2}} object (if \code{returnPlotOnly} is TRUE),
#' or an object containing that \code{\link{ggplot2}} object and intermediate
#' products.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{confIntProp}} and \code{\link{binom.test}}
#' @keywords hplot graphs
#' @examples
#' 
#' ### V/S (no idea what this is: ?mtcars only mentions 'V/S' :-))
#' ### and transmission (automatic vs manual)
#' ggProportionPlot(mtcars, items=c('vs', 'am'));
#' 
#' ### Number of cylinders, by default comparing lowest value
#' ### (4) to highest (8):
#' ggProportionPlot(mtcars, items=c('cyl'));
#' 
#' \dontrun{
#' ### Not running these to save time during package building/checking
#' 
#' ### We can also compare 4 to 6:
#' ggProportionPlot(mtcars, items=c('cyl'),
#'                  hiCategory=6);
#' 
#' ### Now compared to total records, instead of to 
#' ### highest value (hiCategory is ignored then)
#' ggProportionPlot(mtcars, items=c('cyl'),
#'                  compareHiToLo=FALSE);
#' 
#' ### And for 6 cylinders:
#' ggProportionPlot(mtcars, items=c('cyl'),
#'                  loCategory=6, compareHiToLo=FALSE);
#' 
#' ### And for 8 cylinders:
#' ggProportionPlot(mtcars, items=c('cyl'),
#'                  loCategory=8, compareHiToLo=FALSE);
#' 
#' ### And for 8 cylinders with different labels
#' ggProportionPlot(mtcars, items=c('cyl'),
#'                  loCategory=8,
#'                  subQuestions='Cylinders',
#'                  leftAnchors="Eight",
#'                  rightAnchors="Four\nor\nsix",
#'                  compareHiToLo=FALSE);
#' 
#' ### ... And showing the diamonds for the confidence intervals
#' ggProportionPlot(mtcars, items=c('cyl'),
#'                  loCategory=8,
#'                  subQuestions='Cylinders',
#'                  leftAnchors="Eight",
#'                  rightAnchors="Four\nor\nsix",
#'                  compareHiToLo=FALSE,
#'                  showDiamonds=TRUE);
#' }
#' 
#' ### Using less steps for the confidence levels and changing
#' ### the fill colours
#' ggProportionPlot(mtcars,
#'                  items=c('vs', 'am'),
#'                  showDiamonds = TRUE,
#'                  scale_fill = c("#B63679FF", "#FCFDBFFF"),
#'                  conf.steps=seq(from=0.0001, to=.9999, by=.2));
#' 
#' 
#' @export ggProportionPlot
ggProportionPlot <- function(dat,
                             items=NULL,
                             loCategory = NULL,
                             hiCategory = NULL,
                             subQuestions = NULL,
                             leftAnchors = NULL,
                             rightAnchors = NULL,
                             compareHiToLo = TRUE,
                             showDiamonds = FALSE,
                             diamonds.conf.level=.95,
                             diamonds.alpha=1,
                             na.rm = TRUE,
                             barHeight = .4,
                             conf.steps = seq(from=0.001, to=.999, by=.001),
                             scale_color = viridis(option="magma", 2, begin=0, end=.5),
                             scale_fill = viridis(option="magma", 2, begin=0, end=.5),
                             linetype = 1,
                             theme = theme_bw(),
                             returnPlotOnly = TRUE) {

  if (is.vector(dat) || is.factor(dat)) {
    if (is.character(dat)) {
      if (all(grepl('\\d+', colnames(dat)))) {
        dat <- as.numeric(dat);
      } else {
        dat <- as.factor(dat);
      }
    }
    tmpDat <- data.frame(dat);
    items <- deparse(substitute(dat));
    colnames(tmpDat) <- items;
    if (is.factor(dat)) {
      loCategory <- levels(dat)[1];
      hiCategory <- levels(dat)[length(levels(dat))];
    } else {
      loCategory <- min(tmpDat[, ]);
      hiCategory <- max(tmpDat[, ]);
    }
  } else if (is.data.frame(dat)) {
    if (is.null(items)) {
      items <- names(dat);
    }
    
    if (!all(items %in% names(dat))) {
      stop("You specified items that do not exist in the data you provided (specifically, ",
           vecTxtQ(items[!items %in% names(dat)]), ").");
    }
    
    tmpDat <- dat[, items, drop=FALSE];
    if (!all(unlist(lapply(tmpDat, is.numeric)))) {
      warning("You provided one or more factors; this may be problematic ");
    } else {
      if (is.null(loCategory)) {
        loCategory <- min(dat[, items], na.rm=TRUE);
      }
      if (is.null(hiCategory)) {
        hiCategory <- max(dat[, items], na.rm=TRUE);
      }
    }
  }
  if (na.rm) {
    tmpDat <- tmpDat[complete.cases(tmpDat), , drop=FALSE];
  }
  
  if (is.null(subQuestions)) {
    subQuestions <- items;
  }
  if (is.null(leftAnchors)) {
    leftAnchors <- rep(loCategory, length(items));
  }
  if (is.null(rightAnchors)) {
    if (compareHiToLo) {
      rightAnchors <- rep(hiCategory, length(items));
    } else {
      rightAnchors <- rep("Rest", length(items));
    }
  }

  if (ncol(tmpDat) == 1) {
    freqs <- data.frame(loCategoryCount = sum(tmpDat[, items] == loCategory));
  } else {
    freqs <- data.frame(loCategoryCount = colSums(tmpDat[, items] == loCategory));
  }

  if (compareHiToLo) {
    if (ncol(tmpDat) == 1) {
      freqs$totals <- sum(tmpDat[, items] == hiCategory) + freqs$loCategoryCount;
    } else {
      freqs$totals <- colSums(tmpDat[, items] == hiCategory) + freqs$loCategoryCount;
    }
  } else {
    freqs$totals <- rep(nrow(tmpDat), nrow(freqs));
  }

  confidences <- apply(freqs, 1, function(x) {
    #return(pbeta(conf.steps, x['loCategoryCount'], x['totals'] - x['loCategoryCount'] + 1));
    fullCIs <- confIntProp(x=x['loCategoryCount'],
                           n=x['totals'],
                           conf.level = conf.steps);
    return(c(rev(fullCIs[, 1]), fullCIs[, 2]));
  });
  
  ### Adjust confidence steps to conveniently generate
  ### a gradient. Add zero to the beginning to get the right
  ### number of intervals to draw the rectangles.
  conf.steps.adjusted <- c(0, conf.steps/2, rev(1-(conf.steps/2)));
  ### Get the beginning and end positions for the rectangles
  percentages.x1 <- rbind(0, 100*confidences);
  percentages.x2 <- rbind(100*confidences, 100);
  
  longDat <- data.frame(Confidence=rep(conf.steps.adjusted, length(items)),
                        PercentageMin=as.vector(percentages.x1),
                        PercentageMax=as.vector(percentages.x2),
                        item=as.factor(rep(items, each=nrow(percentages.x1))));

  longDat$itemValue <- as.numeric(longDat$item);
  longDat$itemValueMin <- longDat$itemValue - barHeight;
  longDat$itemValueMax <- longDat$itemValue + barHeight;

  plot <- ggplot(longDat, aes_string(x='PercentageMin',
                                     y='itemValue')) +
    geom_rect(aes_string(xmin='PercentageMin',
                         xmax='PercentageMax',
                         ymin='itemValueMin',
                         ymax='itemValueMax',
                         color='Confidence',
                         fill='Confidence'),
              linetype=linetype,
              show.legend=FALSE) +
    scale_y_continuous(breaks=sort(unique(longDat$itemValue)),
                       minor_breaks = NULL,
                       labels=leftAnchors,
                       name=NULL,
                       sec.axis = dup_axis(labels=rightAnchors)) +
    coord_cartesian(xlim=c(0, 100)) +
    theme +
    scale_color_gradient(low=scale_color[1],
                         high=scale_color[2]) +
    scale_fill_gradient(low=scale_fill[1],
                        high=scale_fill[2]);

  if (showDiamonds) {
    confInts <- t(apply(freqs, 1, function(x) {
        return(confIntProp(x['loCategoryCount'], x['totals'],
                           conf.level=diamonds.conf.level));
      }));
    colnames(confInts) <- c('ci.lo', 'ci.hi');
    confInts <- data.frame(100 * confInts);
    confInts$percentage <- 100 * freqs$loCategoryCount / freqs$totals;
    confInts <- confInts[, c(1,3,2)];
    confInts$otherAxisCol <- rev(1:length(items));
    plot <- plot + ggDiamondLayer(confInts,
                                  otherAxisCol='otherAxisCol',
                                  alpha=diamonds.alpha);
  }
  
  ### Create new version with subquestion labels
  suppressMessages(subQuestionLabelplot <- plot +
    scale_y_continuous(breaks=sort(unique(longDat$itemValue)),
                       minor_breaks = NULL,
                       name=NULL,
                       labels=subQuestions,
                       sec.axis=dup_axis(labels=subQuestions)) +
    theme(axis.text.y = element_text(size=rel(1.25), color="black"),
          axis.ticks.y = element_blank()));

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

  if (returnPlotOnly) {
    return(fullPlot);
  } else {
    res <- list(plot = fullPlot,
                confidences = confidences,
                longDat = longDat);
    class(res) <- c('ggProportionPlot');
    return(res);
  }
}

print.ggProportionPlot <- function(x, ...) {
  ### Empty canvas
  grid.newpage();
  ### Draw plot
  grid.draw(x$plot);
}

grid.draw.ggProportionPlot <- function(x, ...) {
  ### Draw plot
  grid.draw(x$plot);
}
