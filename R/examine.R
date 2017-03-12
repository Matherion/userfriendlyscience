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
