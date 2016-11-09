meanSDtoDiamondPlot <- function(dat = NULL,
                                means = 1, sds = 2, ns = 3,
                                labels = NULL,
                                colorCol=NULL,
                                conf.level=.95,
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
  
  if (!is.null(colorCol))
    tmpDf[, 5] <- dat[, 5];

  names(tmpDf) <- NULL;
  rownames(tmpDf) <- NULL;

  if (is.null(colorCol)) {
    return(diamondPlot(tmpDf, yValues=4, yLabels=4, ...));
  } else {
    return(diamondPlot(tmpDf, yValues=4, yLabels=4, colorCol=5, ...));
  }

}
