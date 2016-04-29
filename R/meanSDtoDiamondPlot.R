meanSDtoDiamondPlot <- function(dat = NULL, means = 1, sds = 2, ns = 3, labels = 4, conf.level=.95) {
  
  if (!is.null(dat)) {
    if (!is.null(labels)) {
      dat <- na.omit(dat[, c(means, sds, ns, labels)]);
      labels <- dat[, labels];
    } else {
      dat <- na.omit(dat[, c(means, sds, ns)]);
      labels <- 1:nrow(dat);
    }
    means <- dat[, means];
    sds <- dat[, sds];
    ns <- dat[, ns];
  } else {
    if (means == 1 && sds == 2 && ns == 3 && labels == 4) {
      stop(paste0("If no datafile is specified, specify vectors or values for the ",
                  "means, standard deviations, and sample sizes!"));
    }
  }
  
  tmpDf <- data.frame(meanConfInt(mean = means,
                                  sd = sds,
                                  n = ns)$output$ci);
  tmpDf[, 3] <- tmpDf[, 2];
  tmpDf[, 2] <- means;
  tmpDf[, 4] <- labels;
  rownames(tmpDf) <- NULL;

  return(diamondPlot(tmpDf, yValues=4));

}
