associationsToDiamondPlotDf <- function(dat, covariates, criterion,
                                        labels = NULL,
                                        decreasing=NULL,
                                        conf.level=.95,
                                        esMetric = 'r') {

  if (is.null(labels)) labels <- covariates;

  assocMatrix <- associationMatrix(dat, x=covariates, y=criterion);

  resDf <- data.frame(lo = as.numeric(assocMatrix$output$raw$ci.lo),
                      es = as.numeric(assocMatrix$output$raw$es),
                      hi = as.numeric(assocMatrix$output$raw$ci.hi));

  if (esMetric == 'r') {
    resDf <- data.frame(matrix(sapply(1:length(covariates), function(i) {
      if (assocMatrix$output$raw$esType[i] == 'd') {
        return(convert.d.to.r(resDf[i, ]));
      } else if ((assocMatrix$output$raw$esType[i] == 'etasq') ||
                 (assocMatrix$output$raw$esType[i] == 'omegasq')) {
        return(sqrt(resDf[i, ]));
      } else {
        return(resDf[i, ]);
      }
    }), ncol=3, byrow=TRUE));
  } else if (esMetric == 'd' | esMetric == 'g') {
    resDf <- data.frame(matrix(sapply(1:length(covariates), function(i) {
      if (assocMatrix$output$raw$esType[i] == 'r' | assocMatrix$output$raw$esType[i] == 'v') {
        return(convert.r.to.d(resDf[i, ]));
      } else if ((assocMatrix$output$raw$esType[i] == 'etasq') ||
                 (assocMatrix$output$raw$esType[i] == 'omegasq')) {
        return(convert.r.to.d(sqrt(resDf[i, ])));
      } else {
        return(resDf[i, ]);
      }
    }), ncol=3, byrow=TRUE));
  } else {
    stop("No other effect size metrics implemented yet!");
  }

  names(resDf) <- c('lo', 'es', 'hi');
  resDf$label <- labels;
  resDf$rownr <- 1:nrow(resDf);
  resDf$constant <- 1;

  if (!is.null(decreasing)) {
    ### Invert 'decreasing' because ggplot plots the lowest/first value first (near the origin).
    ### So a decreasing sort would normally result in higher means being displayed LOWER in
    ### the plot, which is counter-intuitive, hence the inversion.
    sortedByMean <- order(unlist(resDf$es), decreasing=!decreasing);
    resDf <- resDf[sortedByMean, ];
    labels <- labels[sortedByMean];
  } else {
    ### sortedByMean is used later on to organise the raw data; therefore, this should
    ### reflect the order of the variables on the Y axis regardless of whether they're
    ### reorganised
    sortedByMean <- 1:length(labels);
  }

  ### Return this vector as attribute to use in meansDiamondPlot
  attr(resDf, 'sortedByMean') <- sortedByMean;

  return(resDf);

}
