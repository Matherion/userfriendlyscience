meanSDtoDiamondPlot <- function(dat = NULL,
                                means = 1, sds = 2, ns = 3,
                                labels = NULL,
                                colorCol=NULL,
                                conf.level=.95,
                                xlab='Means',
                                outputFile = NULL,
                                outputWidth = 10,
                                outputHeight = 10,
                                ggsaveParams = list(units='cm',
                                                    dpi=300,
                                                    type="cairo"),
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
  tmpDf[, 5] <- 1:nrow(tmpDf);
        
  if (!is.null(colorCol))
    tmpDf[, 6] <- dat[, colorCol];

  names(tmpDf) <- NULL;
  rownames(tmpDf) <- NULL;

  if (is.null(colorCol)) {
    plot <- diamondPlot(tmpDf, yLabels=4, yValues=5, xlab=xlab, ...);
  } else {
    plot <- diamondPlot(tmpDf, yLabels=4, yValues=5, colorCol=6, xlab=xlab, ...);
  }

  if (!is.null(outputFile)) {
    ggsaveParameters <- c(list(filename = outputFile,
                               plot = plot,
                               width = outputWidth,
                               height = outputHeight),
                          ggsaveParams);
    do.call(ggsave, ggsaveParameters);
  }
  
  return(plot);

}
