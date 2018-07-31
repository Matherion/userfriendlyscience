#' A diamond plot based on means, standard deviations, and sample sizes
#' 
#' This function generates a so-called diamond plot: a plot based on the forest
#' plots that are commonplace in meta-analyses. The underlying idea is that
#' point estimates are uninformative, and it would be better to focus on
#' confidence intervals. The problem of the points with errorbars that are
#' commonly employed is that the focus the audience's attention on the upper
#' and lower bounds, even though those are the least relevant values. Using
#' diamonds remedies this.
#' 
#' 
#' @param dat The dataset containing the means, standard deviations, sample
#' sizes, and possible labels and manually specified colors.
#' @param means Either the column in the dataframe containing the means, as
#' numeric or as character index, or a vector of means.
#' @param sds Either the column in the dataframe containing the standard
#' deviations, as numeric or as character index, or a vector of standard
#' deviations.
#' @param ns Either the column in the dataframe containing the sample sizes, as
#' numeric or as character index, or a vector of sample sizes.
#' @param labels Optionally, either the column in the dataframe containing
#' labels, as numeric or as character index, or a vector of labels.
#' @param colorCol Optionally, either the column in the dataframe containing
#' manually specified colours, as numeric or as character index, or a vector of
#' manually specified colours.
#' @param conf.level The confidence of the confidence intervals.
#' @param xlab The label for the x axis.
#' @param outputFile A file to which to save the plot.
#' @param outputWidth,outputHeight Width and height of saved plot (specified in
#' centimeters by default, see \code{ggsaveParams}).
#' @param ggsaveParams Parameters to pass to ggsave when saving the plot.
#' @param \dots Additional arguments are passed to \code{\link{diamondPlot}}
#' and eventually to \code{\link{ggDiamondLayer}}. This can be used to, for
#' example, specify two or more colors to use to generate a gradient (using
#' \code{generateColors} and maybe \code{fullColorRange}).
#' @return A \code{\link{ggplot}} plot with a \code{\link{ggDiamondLayer}} is
#' returned.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{meansDiamondPlot}}, \code{\link{diamondPlot}},
#' \code{\link{factorLoadingDiamondCIplot}}, \code{\link{ggDiamondLayer}}
#' @keywords hplot
#' @examples
#' 
#' tmpDf <- data.frame(means = c(1, 2, 3),
#'                     sds = c(1.5, 3, 5),
#'                     ns = c(2, 4, 10),
#'                     labels = c('first', 'second', 'third'),
#'                     color = c('purple', 'grey', 'orange'));
#'                     
#' ### A simple diamond plot
#' meanSDtoDiamondPlot(tmpDf);
#' 
#' ### A simple diamond plot with labels
#' meanSDtoDiamondPlot(tmpDf, labels=4);
#' 
#' ### When specifying column names, specify column
#' ### names for all columns
#' meanSDtoDiamondPlot(tmpDf, means='means',
#'                     sds='sds', ns='ns',
#'                     labels='labels');
#' 
#' ### A diamond plot using the specified colours
#' meanSDtoDiamondPlot(tmpDf, labels=4, colorCol=5);
#' 
#' ### A diamond plot using automatically generated colours
#' ### using a gradient
#' meanSDtoDiamondPlot(tmpDf,
#'                     generateColors=c('green', 'red'));
#' 
#' ### A diamond plot using automatically generated colours
#' ### using a gradient, specifying the minimum and maximum
#' ### possible values that can be attained
#' meanSDtoDiamondPlot(tmpDf,
#'                     generateColors=c('red', 'yellow', 'blue'),
#'                     fullColorRange=c(0, 5));
#' 
#' @export meanSDtoDiamondPlot
meanSDtoDiamondPlot <- function(dat = NULL,
                                means = 1, sds = 2, ns = 3,
                                labels = NULL,
                                colorCol=NULL,
                                conf.level=.95,
                                xlab='Means',
                                outputFile = NULL,
                                outputWidth = 10,
                                outputHeight = 10,
                                ggsaveParams = list(units='cm',
                                                    dpi=300,
                                                    type="cairo"),
                                ...) {

  varNamesToUse <- c(means, sds, ns);
  if (!is.null(labels))
    varNamesToUse <- c(varNamesToUse, labels);
  if (!is.null(colorCol))
    varNamesToUse <- c(varNamesToUse, colorCol);

  if (!is.null(dat)) {
    if (!is.null(labels)) {
      dat <- na.omit(dat[, varNamesToUse]);
      labels <- dat[, 4];
    } else {
      dat <- na.omit(dat[, varNamesToUse]);
      labels <- 1:nrow(dat);
    }
    means <- dat[, 1];
    sds <- dat[, 2];
    ns <- dat[, 3];
  } else {
    if (means == 1 && sds == 2 && ns == 3) {
      stop(paste0("If no datafile is specified, specify vectors or values for the ",
                  "means, standard deviations, and sample sizes!"));
    }
    if ((length(means) != length(sds)) || (length(means) != length(ns))) {
      stop("Vectors 'means', 'sds', and 'ns' have to have the same length!");
    }
  }

  tmpDf <- data.frame(meanConfInt(mean = means,
                                  sd = sds,
                                  n = ns,
                                  conf.level = conf.level)$output$ci);
  tmpDf[, 3] <- tmpDf[, 2];
  tmpDf[, 2] <- means;
  tmpDf[, 4] <- labels;
  tmpDf[, 5] <- 1:nrow(tmpDf);
        
  if (!is.null(colorCol))
    tmpDf[, 6] <- dat[, colorCol];

  names(tmpDf) <- NULL;
  rownames(tmpDf) <- NULL;

  if (is.null(colorCol)) {
    plot <- diamondPlot(tmpDf, yLabels=4, yValues=5, xlab=xlab, ...);
  } else {
    plot <- diamondPlot(tmpDf, yLabels=4, yValues=5, colorCol=6, xlab=xlab, ...);
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
