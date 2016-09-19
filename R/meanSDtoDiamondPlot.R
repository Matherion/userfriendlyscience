meanSDtoDiamondPlot <- function(dat = NULL, means = 1, sds = 2, ns = 3, labels = 4, conf.level=.95) {

  if (!is.null(dat)) {
    if (!is.null(labels)) {
      dat <- na.omit(dat[, c(means, sds, ns, labels)]);
      labels <- dat[, 4];
    } else {
      dat <- na.omit(dat[, c(means, sds, ns)]);
      labels <- 1:nrow(dat);
    }
    means <- dat[, 1];
    sds <- dat[, 2];
    ns <- dat[, 3];
  } else {
    if (means == 1 && sds == 2 && ns == 3 && labels == 4) {
      stop(paste0("If no datafile is specified, specify vectors or values for the ",
                  "means, standard deviations, and sample sizes!"));
    }
  }

  tmpDf <- data.frame(meanConfInt(mean = means,
                                  sd = sds,
                                  n = ns,
                                  conf.level = conf.level)$output$ci);
  tmpDf[, 3] <- tmpDf[, 2];
  tmpDf[, 2] <- means;
  tmpDf[, 4] <- labels;
  names(tmpDf) <- NULL;
  rownames(tmpDf) <- NULL;

  return(diamondPlot(tmpDf, yValues=4));

}
