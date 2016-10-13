examine <- function(..., stem=TRUE, plots=TRUE,
                    extremeValues = 5, descr.include=NULL,
                    qqCI=TRUE, conf.level=.95) {

  varNames <- NULL;
  
  if (length(list(...)) == 1) {
    dat <- list(...)[[1]];
    if (is.data.frame(dat)) {
      vectorList <- dat;
      varNames <- names(dat);
    } else {
      vectorList <- list(...);
    }
  } else {
    vectorList <- list(...);
  }
  
  if (is.null(varNames)) {
    varNames <- unlist(as.list(substitute(list(...)))[-1]);
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
      rsl$xtrm <- list(lo = head(tmpDf, extremeValues),
                       hi = tail(tmpDf, extremeValues));
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
    print(x[[currentName]]$descr);
    if (!is.null(x[[currentName]]$xtrm)) {
      cat("###### Lowest values:\n");
      print(x[[currentName]]$xtrm$lo[, 'value', drop=FALSE]);
      cat("\n###### Highest values:\n");
      print(x[[currentName]]$xtrm$hi[, 'value', drop=FALSE]);
    }
    if (!is.null(x[[currentName]]$stem)) {
      cat("\n###### Stem and leaf plot:\n", x[[currentName]]$stem);
    }
    cat("\n");
    if (!is.null(x[[currentName]]$dataShapePlot)) {
      grid.arrange(textGrob(paste0('Histogram, Q-Q plot & boxplot for ', extractVarName(currentName)),
                            gp=gpar(fontsize=14)),
                   x[[currentName]]$dataShapePlot,
                   ncol=1, heights=c(.1, .9));
    }
  }
}
