#' meansComparisonDiamondPlot and duoComparisonDiamondPlot
#' 
#' These are two diamond plot functions to conveniently make diamond plots to
#' compare subgroups or different samples. They are both based on a univariate
#' diamond plot where colors are used to distinguish the data points and
#' diamonds of each subgroup or sample. The means comparison diamond plot
#' produces only this plot, while the duo comparison diamond plot combines it
#' with a diamond plot visualising the effect sizes of the associations. The
#' latter currently only works for two subgroups or samples, while the simple
#' meansComparisonDiamondPlot also works when comparing more than two sets of
#' datapoints. These functions are explained more in detail in Peters (2017).
#' 
#' These functions are explained in Peters (2017).
#' 
#' @aliases meansComparisonDiamondPlot duoComparisonDiamondPlot
#' @param dat The dataframe containing the relevant variables.
#' @param items The variables to plot (on the y axis).
#' @param compareBy The variable by which to compare (i.e. the variable
#' indicating to which subgroup or sample a row in the dataframe belongs).
#' @param labels The labels to use on the y axis; these values will replace the
#' variable names in the dataframe (specified in \code{items}).
#' @param compareByLabels The labels to use to replace the value labels of the
#' \code{compareBy} variable.
#' @param decreasing Whether to sort the variables by their mean values
#' (\code{NULL} to not sort, \code{TRUE} to sort in descending order (i.e.
#' items with lower means are plotted more to the bottom), and \code{FALSE} to
#' sort in ascending order (i.e. items with lower means are plotted more to the
#' top).
#' @param sortBy If the variables should be sorted (see \code{decreasing}),
#' this variable specified which subgroup should be sorted by. Therefore, the
#' value specified here must be a value label ('level label') of the
#' \code{comparisonBy} variable.
#' @param conf.level The confidence level of the confidence intervals specified
#' by the diamonds for the means (for \code{meansComparisonDiamondPlot}) and
#' for both the means and effect sizes (for \code{duoComparisonDiamondPlot}).
#' @param showData Whether to plot the data points.
#' @param dataAlpha The transparency (alpha channel) value for the data points:
#' a value between 0 and 1, where 0 denotes complete transparency and 1 denotes
#' complete opacity.
#' @param dataSize The size of the data points.
#' @param comparisonColors The colors to use for the different subgroups or
#' samples. This should be a vector of valid colors with at least as many
#' elements as sets of data points that should be plotted.
#' @param associationsColor For \code{duoComparisonDiamondPlot}, the color to
#' use to plot the effect sizes in the right-hand plot.
#' @param alpha The alpha channel (transparency) value for the diamonds: a
#' value between 0 and 1, where 0 denotes complete transparency and 1 denotes
#' complete opacity.
#' @param jitterWidth,jitterHeight How much noise to add to the data points (to
#' prevent overplotting) in the horizontal (x axis) and vertical (y axis)
#' directions.
#' @param xlab,ylab The label to use for the x and y axes (for
#' \code{duoComparisonDiamondPlot}, must be vectors of two elements). Use
#' \code{NULL} to not use a label.
#' @param theme The theme to use for the plots.
#' @param showLegend Whether to show the legend (which color represents which
#' subgroup/sample).
#' @param lineSize The thickness of the lines (the diamonds' strokes).
#' @param drawPlot Whether to draw the plot, or only (invisibly) return it.
#' @param xbreaks Where the breaks (major grid lines, ticks, and labels) on the
#' x axis should be.
#' @param outputFile A file to which to save the plot.
#' @param outputWidth,outputHeight Width and height of saved plot (specified in
#' centimeters by default, see \code{ggsaveParams}).
#' @param ggsaveParams Parameters to pass to ggsave when saving the plot.
#' @param \dots Any additional arguments are passed to
#' \code{\link{diamondPlot}} by \code{meansComparisonDiamondPlot} and to both
#' \code{meansComparisonDiamondPlot} and \code{\link{associationsDiamondPlot}}
#' by \code{duoComparisonDiamondPlot}.
#' @return Diamond plots: a \code{\link{ggplot}} by
#' \code{meansComparisonDiamondPlot}, and a \code{\link{gtable}} by
#' \code{duoComparisonDiamondPlot}.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{diamondPlot}}, \code{\link{meansDiamondPlot}},
#' \code{\link{CIBER}}
#' @references Peters, G.-J. Y. (2017). Diamond Plots: a tutorial to introduce
#' a visualisation tool that facilitates interpretation and comparison of
#' multiple sample estimates while respecting their inaccuracy.
#' \emph{PsyArXiv.} https://doi.org/10.17605/OSF.IO/9W8YV
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' meansComparisonDiamondPlot(mtcars,
#'                            items=c('disp', 'hp'),
#'                            compareBy='vs',
#'                            xbreaks=c(100,200, 300, 400));
#' meansComparisonDiamondPlot(chickwts,
#'                            items='weight',
#'                            compareBy='feed',
#'                            xbreaks=c(100,200,300,400),
#'                            showData=FALSE);
#' duoComparisonDiamondPlot(mtcars,
#'                          items=c('disp', 'hp'),
#'                          compareBy='vs',
#'                          xbreaks=c(100,200, 300, 400));
#' 
#' @export meansComparisonDiamondPlot
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
                                       ylab=NULL,
                                       theme=theme_bw(),
                                       showLegend=TRUE,
                                       lineSize=1,
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
            decreasing = decreasing);

    ### Invert because ggplot plots first elements on y axis lowest
    res$intermediate$sortOrder <- rev(res$intermediate$sortOrder);

    res$intermediate$dat <- lapply(res$intermediate$dat,
                                   function(df, s = res$intermediate$sortOrder) {
                                     return(df[s, ]);
                                   });
  } else {
    res$intermediate$sortOrder <- 1:nrow(res$intermediate$dat[[1]]);

    ### Invert because ggplot plots first elements on y axis lowest
    res$intermediate$sortOrder <- rev(res$intermediate$sortOrder);

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
      ### Note that we revert the order here again, because
      ### rawData uses the order as provided (whereas some
      ### other functions invert it)
      res$intermediate$dataLayers[[compareByLabels[i]]] <-
        rawDataDiamondLayer(res$intermediate$rawDat[[compareByLabels[i]]],
                            items=items,
                            itemOrder = rev(res$intermediate$sortOrder),
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

  if (!is.null(xbreaks) &&
      length(xbreaks) == 1 &&
      tolower(xbreaks) == "auto") {
    plot <- plot + scale_x_continuous(breaks=sort(unique(unlist(dat[, items]))));
  } else if (is.numeric(xbreaks)) {
    plot <- plot + scale_x_continuous(breaks=xbreaks);
  }

  if (!is.null(outputFile)) {
    ggsaveParameters <- c(list(filename = outputFile,
                               plot = plot,
                               width = outputWidth,
                               height = outputHeight),
                          ggsaveParams);
    do.call(ggsave, ggsaveParameters);
  }

  attr(plot, 'itemOrder') <- res$intermediate$sortOrder;

  return(plot);
}

