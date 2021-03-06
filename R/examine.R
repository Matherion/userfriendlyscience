#' Examine one or more variables
#' 
#' These functions are one of many R functions enabling users to assess
#' variable descriptives. They have been developed to mimic SPSS' 'EXAMINE'
#' syntax command ('Explore' in the menu) as closely as possible to ease the
#' transition for new R users and facilitate teaching courses where both
#' programs are taught alongside each other.
#' 
#' This function basically just calls the \code{\link{descr}} function,
#' optionally supplemented with calls to \code{\link{stem}},
#' \code{\link{dataShape}}.
#' 
#' @aliases examine examineBy
#' @param \dots The first argument is a list of variables to provide
#' descriptives for. Because these are the first arguments, the other arguments
#' must be named explicitly so R does not confuse them for something that
#' should be part of the dots.
#' @param by A variable by which to split the dataset before calling
#' \code{\link{examine}}. This can be used to show the descriptives separate by
#' levels of a factor.
#' @param stem Whether to display a stem and leaf plot.
#' @param plots Whether to display the plots generated by the
#' \code{\link{dataShape}} function.
#' @param extremeValues How many extreme values to show at either end (the
#' highest and lowest values). When set to FALSE (or 0), no extreme values are
#' shown.
#' @param qqCI Whether to display confidence intervals in the QQ-plot.
#' @param descr.include Which descriptives to include; see \code{\link{descr}}
#' for more information.
#' @param conf.level The level of confidence of the confidence interval.
#' @return A list that is displayed when printed.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{descr}}, \code{\link{dataShape}}, \code{\link{stem}}
#' @keywords univar
#' @examples
#' 
#' ### Look at the miles per gallon descriptives:
#' examine(mtcars$mpg, stem=FALSE, plots=FALSE);
#' 
#' ### Separate for the different number of cylinders:
#' examineBy(mtcars$mpg, by=mtcars$cyl,
#'           stem=FALSE, plots=FALSE,
#'           extremeValues=FALSE,
#'           descr.include=c('central tendency', 'spread'));
#' 
#' 
#' @export examine
examine <- function(..., stem=TRUE, plots=TRUE,
                    extremeValues = 5, descr.include=NULL,
                    qqCI=TRUE, conf.level=.95) {

  originalVarNames <- unlist(as.list(substitute(list(...)))[-1]);
  dotList <- list(...);

  if (!is.null(dotList$plot) && dotList$plot) {
    plots <- TRUE;
    dotList$plot <- NULL;
    originalVarNames$plot <- NULL;
  }

  varNames <- NULL;

  if (length(dotList) == 1) {
    dat <- dotList[[1]];
    if (is.data.frame(dat)) {
      vectorList <- dat;
      varNames <- names(dat);
    } else {
      vectorList <- dotList;
    }
  } else {
    vectorList <- dotList;
  }

  if (is.null(varNames)) {
    varNames <- originalVarNames;
  }

  ### Call functions to explore the variables
  res <- lapply(vectorList, function(x) {
    rsl <- list();

    if (is.null(descr.include)) {
      rsl$descr <- descr(x, conf.level=conf.level);
    } else {
      rsl$descr <- descr(x, conf.level=conf.level, include=descr.include);
    }

    tmpDf <- data.frame(rowNr = 1:length(x), value=x);
    tmpDf <- tmpDf[order(x), ];

    if (extremeValues) {
      rsl$xtrm <- list(lo = head(na.omit(tmpDf), extremeValues),
                       hi = tail(na.omit(tmpDf), extremeValues));
    }

    if (plots) {
      if (is.numeric(x)) {
        rsl$dataShapePlot <- dataShape(x, qqCI=qqCI)$output$plot;
      } else {
        rsl$dataShapePlot <- ggBarChart(x);
      }
    }

    if (stem) {
      rsl$stem <- paste0(capture.output(stem(as.numeric(x))), collapse="\n");
    }

    return(rsl);
  });

  ### Set the variable names
  names(res) <- varNames;

  ### Store them as variable names in the descriptives objects
  for (index in 1:length(res)) {
    attr(res[[index]]$descr, 'varName') <- names(res)[index];
  }

  # if (length(res) == 1) {
  #   res <- res[[1]];
  # }

  ### Set class for correct printing and return result
  class(res) <- 'examine';
  return(res);
}

print.examine <- function(x, ...) {
  for (currentName in names(x)) {
    if (class(x[[currentName]]$descr) == 'freq') {
      cat0("###### Frequencies for ", currentName, ":\n\n");
    }
    print(x[[currentName]]$descr);
    if (!is.null(x[[currentName]]$xtrm)) {
      cat0("\n");
      cat("###### Rows with lowest values:\n");
      print(x[[currentName]]$xtrm$lo[, 'value', drop=FALSE]);
      cat("\n###### Rows with highest values:\n");
      print(x[[currentName]]$xtrm$hi[, 'value', drop=FALSE]);
    }
    if (!is.null(x[[currentName]]$stem)) {
      cat("\n###### Stem and leaf plot:\n", x[[currentName]]$stem, "\n");
    }
    if (!is.null(x[[currentName]]$dataShapePlot)) {
      grid.arrange(textGrob(paste0('Histogram, Q-Q plot & boxplot for ', extractVarName(currentName)),
                            gp=gpar(fontsize=14)),
                   x[[currentName]]$dataShapePlot,
                   ncol=1, heights=c(.1, .9));
    }
    cat('\n');
  }
}

pander.examine <- function(x, headerPrefix = "", headerStyle = "**",
                           secondaryHeaderPrefix = "",
                           secondaryHeaderStyle="*", ...) {
  for (currentName in names(x)) {
    cat0("\n\n", headerPrefix, headerStyle,
         extractVarName(currentName),
         headerStyle, "\n\n");
    cat(pander(x[[currentName]]$descr,
               headerPrefix=secondaryHeaderPrefix,
               headerStyle=secondaryHeaderStyle));
    if (!is.null(x[[currentName]]$xtrm)) {
      cat("\n\n");
      cat0(secondaryHeaderPrefix, secondaryHeaderStyle,
          "Rows with lowest values:",
          secondaryHeaderStyle, "  \n\n");
      pander(t(x[[currentName]]$xtrm$lo[, 'value', drop=FALSE]));
      cat("\n\n");
      cat0(secondaryHeaderPrefix, secondaryHeaderStyle,
          "Rows with highest values:",
          secondaryHeaderStyle, "  \n\n");
      pander(t(x[[currentName]]$xtrm$hi[, 'value', drop=FALSE]));
    }
    if (!is.null(x[[currentName]]$stem)) {
      cat("\n\n");
      cat0(secondaryHeaderPrefix, secondaryHeaderStyle,
           "Stem and leaf plot:",
           secondaryHeaderStyle, "  \n", x[[currentName]]$stem);
    }
    cat("\n");
    if (!is.null(x[[currentName]]$dataShapePlot)) {
      plotTitle <- ifelse('gTree' %in% class(x[[currentName]]$dataShapePlot),
                          paste0('Histogram, Q-Q plot & boxplot for ', extractVarName(currentName)),
                          paste0('Barchart for ', extractVarName(currentName)));
      grid.arrange(textGrob(plotTitle,
                            gp=gpar(fontsize=14)),
                   x[[currentName]]$dataShapePlot,
                   ncol=1, heights=c(.1, .9));
    }
  }
}
