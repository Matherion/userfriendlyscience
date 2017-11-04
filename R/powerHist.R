### Note: this is necessary to prevent Rcmd CHECK from throwing a note;
### otherwise it think these variable weren't defined yet.
utils::globalVariables(c('distribution', '..density..',
                         '..scaled', 'normalX', 'normalY'));

### Make a 'better' histogram using ggplot
powerHist <- function(vector,
                      histColor = "#0000CC",
                      distributionColor = "#0000CC",
                      normalColor = "#00CC00",
                      distributionLineSize = 1,
                      normalLineSize = 1,
                      histAlpha = .25,
                      xLabel = NULL,
                      yLabel = NULL,
                      normalCurve = TRUE,
                      distCurve = TRUE,
                      breaks = 30,
                      theme=dlvTheme(),
                      rug=NULL, jitteredRug=TRUE, rugSides="b",
                      rugAlpha = .2,
                      returnPlotOnly = FALSE) {
  varName <- deparse(substitute(vector));
  vector <- na.omit(vector);
  if (!is.numeric(vector)) {
    tryCatch(vector <- as.numeric(vector), error = function(e) {
      stop("The vector you supplied is not numeric; I tried to convert it, ",
           "but my attempt failed. The error I got is:\n", e);
    });
  }
  
  ### Create object to return, storing all input variables 
  res <- list(input = as.list(environment()), intermediate = list(), output = list());
  
  res$input$sampleSize = length(vector);
  res$intermediate$distribution <- res$input$vector;

  ### Compute binwidth and scaling factor for density lines
  res$intermediate$tempBinWidth <- (max(res$input$vector) -
                                    min(res$input$vector)) / breaks;
  scalingFactor <- max(table(cut(vector, breaks=breaks)));

  if (normalCurve) {
    res$intermediate$normalX <- c(seq(min(res$input$vector), max(res$input$vector),
                                      by=(max(res$input$vector) -
                                            min(res$input$vector)) /
                                        (res$input$sampleSize-1)));
    res$intermediate$normalY <- dnorm(res$intermediate$normalX,
                                      mean=mean(res$input$vector),
                                      sd=sd(res$input$vector));
    res$intermediate$normalY <- (1 / max(res$intermediate$normalY)) *
      scalingFactor *
      res$intermediate$normalY;
    res$dat <- data.frame(normalX = res$intermediate$normalX,
                          normalY = res$intermediate$normalY,
                          distribution = res$intermediate$distribution);
  } else {
    res$dat <- data.frame(distribution = res$intermediate$distribution);
  }
  
  ### Generate labels if these weren't specified
  if (is.null(xLabel)) {
    xLabel <- paste0('Value of ', extractVarName(varName));
  }
  if (is.null(yLabel)) {
    yLabel <- "Frequency";
  }

  ### Plot distribution
  res$plot <- ggplot(data=res$dat, aes(x=distribution)) + 
    xlab(xLabel) +
    ylab(yLabel) +
    geom_histogram(color=NA, fill=histColor, na.rm = TRUE,
                   alpha=histAlpha, binwidth=res$intermediate$tempBinWidth);
  if (distCurve) {
    res$plot <- res$plot +
    geom_line(aes_q(y=bquote(..scaled.. * .(scalingFactor))),
              stat = 'density',
              color=distributionColor, size=distributionLineSize,
              na.rm = TRUE);
  }
  if (normalCurve) {
    res$plot <- res$plot +
      geom_line(aes(x=normalX, y=normalY), color=normalColor, size=normalLineSize,
                na.rm = TRUE);
  }
  res$plot <- res$plot + theme;

  if (is.null(rug)) {
    if (nrow(res$dat) < 1000) {
      rug <- TRUE;
    } else {
      rug <- FALSE;
    }
  }
  
  if (rug) {
    if (jitteredRug) {
      res$plot <- res$plot + geom_rug(color=distributionColor, sides=rugSides,
                                      aes(y=0), position="jitter",
                                      alpha=rugAlpha,
                                      na.rm = TRUE);
    } else {
      res$plot <- res$plot + geom_rug(color=distributionColor, sides=rugSides,
                                      alpha=rugAlpha,
                                      na.rm = TRUE);
    }
  }
  
  if (!is.null(res$input$xLabel) && is.logical(res$input$xLabel) &&
      !(res$input$xLabel)) {
    res$plot <- res$plot + theme(axis.title.x = element_blank());
  }
  if (!is.null(res$input$yLabel) && is.logical(res$input$yLabel) &&
      !(res$input$yLabel)) {
    res$plot <- res$plot + theme(axis.title.y = element_blank());
  }
  
  if (returnPlotOnly) {
    return(res$plot)
  } else {
    ### Set class and return result
    class(res) <- "powerHist";
    return(res);
  }
}

print.powerHist <- function(x, ...) {
  print(x$plot, ...);
}
