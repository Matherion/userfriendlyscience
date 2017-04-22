associationsDiamondPlot <- function(dat, covariates, criterion,
                                    labels = NULL,
                                    decreasing=NULL,
                                    conf.level=.95,
                                    diamondColors = NULL,
                                    returnLayerOnly = FALSE,
                                    esMetric = 'r',
                                    ...) {

  res <- list(input = as.list(environment()),
              intermediate = list());

  res$intermediate$dat <- associationsToDiamondPlotDf(dat, covariates = covariates,
                                                      criterion = criterion,
                                                      labels = labels,
                                                      decreasing=decreasing,
                                                      conf.level=conf.level,
                                                      esMetric = esMetric);

  ### Get labels from this dataframe, because they may have been sorted
  labels <- res$intermediate$dat$label;

  return(diamondPlot(res$intermediate$dat, ciCols=c('lo', 'es', 'hi'),
                     yLabels = labels, colorCol=diamondColors,
                     returnLayerOnly = returnLayerOnly, ...));

}

