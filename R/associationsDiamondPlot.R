#' A diamondplot with confidence intervals for associations
#' 
#' This function produces is a diamondplot that plots the confidence intervals
#' for associations between a number of covariates and a criterion. It
#' currently only supports the Pearson's r effect size metric; other effect
#' sizes are converted to Pearson's r.
#' 
#' associationsToDiamondPlotDf is a helper function that produces the required
#' dataframe.
#' 
#' This function can be used to quickly plot multiple confidence intervals.
#' 
#' @aliases associationsDiamondPlot associationsToDiamondPlotDf
#' @param dat The dataframe containing the relevant variables.
#' @param covariates The covariates: the list of variables to associate to the
#' criterion or criteria, usually the predictors.
#' @param criteria,criterion The criteria, usually the dependent variables; one
#' criterion (one dependent variable) can also be specified of course. The
#' helper function \code{associationsToDiamondPlotDf} always accepts only one
#' criterion.
#' @param labels The labels for the covariates, for example the questions that
#' were used (as a character vector).
#' @param criteriaLabels The labels for the criteria (in the legend).
#' @param decreasing Whether to sort the covariates by the point estimate of
#' the effect size of their association with the criterion. Use \code{NULL} to
#' not sort at all, \code{TRUE} to sort in descending order, and \code{FALSE}
#' to sort in ascending order.
#' @param sortBy When specifying multiple criteria, this can be used to
#' indicate by which criterion the items should be sorted (if they should be
#' sorted).
#' @param conf.level The confidence of the confidence intervals.
#' @param criteriaColors,criterionColor The colors to use for the different
#' associations can be specified in \code{criteriaColors}. This should be a
#' vector of valid colors with at least as many elements as criteria are
#' specified in \code{criteria}. If only one criterion is specified, the color
#' in \code{criterionColor} is used.
#' @param returnLayerOnly Whether to return the entire object that is
#' generated, or just the resulting ggplot2 layer.
#' @param esMetric The effect size metric to plot - currently, only 'r' is
#' supported, and other values will return an error.
#' @param multiAlpha,singleAlpha The transparency (alpha channel) value of the
#' diamonds for each association can be specified in \code{multiAlpha}, and if
#' only one criterion is specified, the alpha level of the diamonds can be
#' specified in \code{singleAlpha}.
#' @param showLegend Whether to show the legend.
#' @param xlab,ylab The label to use for the x and y axes (for
#' \code{duoComparisonDiamondPlot}, must be vectors of two elements). Use
#' \code{NULL} to not use a label.
#' @param theme The \code{\link{ggplot}} theme to use.
#' @param lineSize The thickness of the lines (the diamonds' strokes).
#' @param outputFile A file to which to save the plot.
#' @param outputWidth,outputHeight Width and height of saved plot (specified in
#' centimeters by default, see \code{ggsaveParams}).
#' @param ggsaveParams Parameters to pass to ggsave when saving the plot.
#' @param \dots Any additional arguments are passed to
#' \code{\link{diamondPlot}} and eventually to \code{\link{ggDiamondLayer}}.
#' @return A plot.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{diamondPlot}}, \code{\link{ggDiamondLayer}},
#' \code{\link{CIBER}}
#' @keywords hplot
#' @examples
#' 
#' ### Simple diamond plot with correlations
#' ### and their confidence intervals
#' 
#' associationsDiamondPlot(mtcars,
#'                         covariates=c('cyl', 'hp', 'drat', 'wt',
#'                                      'am', 'gear', 'vs', 'carb', 'qsec'),
#'                         criteria='mpg');
#' 
#' ### Same diamond plot, but now with two criteria,
#' ### and colouring the diamonds based on the
#' ### correlation point estimates: a gradient
#' ### is created where red is used for -1,
#' ### green for 1 and blue for 0.
#' 
#' associationsDiamondPlot(mtcars,
#'                         covariates=c('cyl', 'hp', 'drat', 'wt',
#'                                      'am', 'gear', 'vs', 'carb', 'qsec'),
#'                         criteria=c('mpg', 'disp'),
#'                         generateColors=c("red", "blue", "green"),
#'                         fullColorRange=c(-1, 1));
#' 
#' @export associationsDiamondPlot
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
                                    xlab="Effect size estimates",
                                    ylab="",
                                    theme=theme_bw(),
                                    lineSize = 1,
                                    outputFile = NULL,
                                    outputWidth = 10,
                                    outputHeight = 10,
                                    ggsaveParams = list(units='cm',
                                                        dpi=300,
                                                        type="cairo"),                                    
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
    if (is.null(sortBy)) sortBy <- criteriaLabels[1];
    ### No idea why this unlist is necessary; for some reason,
    ### using the 'es' index to extract that column returns
    ### a list instead of a vector.
    res$intermediate$sortOrder <-
      order(unlist(res$intermediate$dat[[sortBy]][, 'es']),
            decreasing = decreasing);
    
    ### Invert because ggplot plots first elements on y axis lowest
    res$intermediate$sortOrder <- rev(res$intermediate$sortOrder);
    
    res$intermediate$dat <- lapply(res$intermediate$dat,
                                   function(df, s = res$intermediate$sortOrder) {
                                     return(df[s, ]);
                                   });
  } else {
    ### Invert because ggplot plots first elements on y axis lowest
    res$intermediate$sortOrder <- rev(1:nrow(res$intermediate$dat[[1]]));
  }

  ### Get labels from one of these dataframes,
  ### because they may have been sorted
  labels <- res$intermediate$dat[[1]]$label;

  ### Get diamond layers
  res$intermediate$diamondLayers <- list();
  for (i in 1:length(criteriaLabels)) {
    
    if ('generateColors' %in% names(list(...))) {
      if (length(criteriaLabels) > 1) {
        res$intermediate$diamondLayers[[criteriaLabels[i]]] <-
          diamondPlot(res$intermediate$dat[[criteriaLabels[i]]],
                      ciCols=c('lo', 'es', 'hi'),
                      yLabels = labels,
                      lineColor=ifelse(length(criteria) == 1, criterionColor, criteriaColors[i]),
                      alpha = ifelse(length(criteria) == 1, singleAlpha, multiAlpha),
                      returnLayerOnly = TRUE,
                      size=lineSize, ...);
      } else {
        res$intermediate$diamondLayers[[criteriaLabels[i]]] <-
          diamondPlot(res$intermediate$dat[[criteriaLabels[i]]],
                      ciCols=c('lo', 'es', 'hi'),
                      yLabels = labels,
                      alpha = ifelse(length(criteria) == 1, singleAlpha, multiAlpha),
                      returnLayerOnly = TRUE,
                      size=lineSize, ...);
      }
    } else {
      res$intermediate$diamondLayers[[criteriaLabels[i]]] <-
        diamondPlot(res$intermediate$dat[[criteriaLabels[i]]],
                    ciCols=c('lo', 'es', 'hi'),
                    yLabels = labels,
                    colorCol=ifelse(length(criteria) == 1, criterionColor, criteriaColors[i]),
                    alpha = ifelse(length(criteria) == 1, singleAlpha, multiAlpha),
                    returnLayerOnly = TRUE,
                    size=lineSize, ...);
    }
  }


  ### Only return the layer(s) with diamonds
  if (returnLayerOnly) {
    return(res$intermediate$diamondLayers[[criteriaLabels[i]]]);
  } else {
    plot <- ggplot();
  }

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
  
  if (!is.null(outputFile)) {
    ggsaveParameters <- c(list(filename = outputFile,
                               plot = plot,
                               width = outputWidth,
                               height = outputHeight),
                          ggsaveParams);
    do.call(ggsave, ggsaveParameters);
  }
  
  return(plot);

}

