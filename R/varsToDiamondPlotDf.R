varsToDiamondPlotDf <- function(dat, items = NULL, labels = NULL,
                                decreasing=NULL,
                                conf.level=.95) {

  if (is.null(items)) {
    items <- names(dat);
  } else if (is.numeric(items)) {
    items <- names(dat)[items];
  }
  if (is.null(labels)) labels <- items;

  # resDf <- data.frame(t(sapply(dat[, items, drop=FALSE],
  #                              function(x) {
  #   x <- na.omit(x);
  #   ci <- meanConfInt(x, conf.level=conf.level)$output$ci;
  #   return(data.frame(lo = ci[1], mean = mean(x), hi = ci[2]));
  # })));

  miniDat <- dat[, items, drop=FALSE];
  notNumericVectors <-
    items[which(!unlist(lapply(miniDat, is.numeric)))];
  if (length(notNumericVectors) > 0) {
    stop("Not all items are numeric (",
         ufs::vecTxtQ(notNumericVectors),
         " are not).");
  }
  ### To fix error with mean
  resDf <-
    matrix(unlist(lapply(miniDat,
                         function(x) {
                           x <- na.omit(x);
                           ci <- meanConfInt(x, conf.level=conf.level)$output$ci;
                           return(c(ci[1], mean(x), ci[2]));
                         })),
           byrow=TRUE, ncol=3);
  resDf <- as.data.frame(resDf);
  names(resDf) <- c('lo', 'mean', 'hi');

  resDf$label <- labels;
  resDf$rownr <- 1:nrow(resDf);
  resDf$constant <- 1;

  if (!is.null(decreasing)) {
    ### Invert 'decreasing' because ggplot plots the lowest/first value first (near the origin).
    ### So a decreasing sort would normally result in higher means being displayed LOWER in
    ### the plot, which is counter-intuitive, hence the inversion.
    sortedByMean <- order(unlist(resDf$mean), decreasing=!decreasing);
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
