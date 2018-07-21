normalityAssessment <- function(sampleVector, samples = 10000, digits=2,
                                samplingDistColor = "#2222CC",
                                normalColor = "#00CC00",
                                samplingDistLineSize = 2,
                                normalLineSize = 1,
                                xLabel.sampleDist = NULL,
                                yLabel.sampleDist = NULL,
                                xLabel.samplingDist = NULL,
                                yLabel.samplingDist = NULL,
                                sampleSizeOverride = TRUE) {
  
  ### Create object for returning results
  res <- list(sampleVector.raw = sampleVector,
              sampleVector = sampleVector[complete.cases(sampleVector)],
              sampleSize = length(sampleVector[complete.cases(sampleVector)]),
              samples = samples,
              digits = digits);
  
  ### Construct temporary dataset for
  ### plotting sample distribution
  normalX <- c(seq(min(res$sampleVector), max(res$sampleVector),
                   by=(max(res$sampleVector) - min(res$sampleVector))/(res$sampleSize-1)));
  normalY <- dnorm(normalX, mean=mean(res$sampleVector),
                   sd=sd(res$sampleVector));
  sampleDistY <- res$sampleVector;
  tempDat <- data.frame(normalX = normalX, normalY = normalY, sampleDist = sampleDistY);
  tempBinWidth <- (max(res$sampleVector) - min(res$sampleVector)) / 30;

  ### Generate labels if these weren't specified
  if (is.null(xLabel.sampleDist)) {
    xLabel.sampleDist <- extractVarName(deparse(substitute(sampleVector)));
  }
  if (is.null(yLabel.sampleDist)) {
    yLabel.sampleDist <- paste0('Frequencies for n=', res$sampleSize);
  }
  
  ### Plot sample distribution
  res$plot.sampleDist <- powerHist(tempDat$sampleDist,
                                   xLabel=xLabel.sampleDist,
                                   yLabel=yLabel.sampleDist,
                                   distributionColor=samplingDistColor,
                                   normalColor=normalColor,
                                   distributionLineSize=samplingDistLineSize,
                                   normalLineSize=normalLineSize)$plot +
    ggtitle("Sample distribution");
  
  res$qqPlot.sampleDist <- ggqq(tempDat$sampleDist);
  
  ### Take 'samples' samples of sampleSize people and store the means
  ### (first generate an empty vector to store the means)
  res$samplingDistribution <- replicate(samples,
                                        mean(sample(res$sampleVector,
                                                    size=res$sampleSize,
                                                    replace=TRUE)));
  # res$samplingDistribution <- c();
  # for (i in 1:samples) {
  #   res$samplingDistribution[i] <- mean(sample(res$sampleVector, size=res$sampleSize,
  #                                              replace=TRUE));
  # }
  
  ### Construct temporary dataset for
  ### plotting sampling distribution  
  normalX <- c(seq(min(res$samplingDistribution), max(res$samplingDistribution),
                   by=(max(res$samplingDistribution) - min(res$samplingDistribution))/(res$samples-1)));
  normalY <- dnorm(normalX, mean=mean(res$samplingDistribution),
                   sd=sd(res$samplingDistribution));
  samplingDistY <- res$samplingDistribution;
  tempDat <- data.frame(normalX = normalX, normalY = normalY, samplingDist = samplingDistY);
  tempBinWidth <- (max(res$samplingDistribution) - min(res$samplingDistribution)) / 30;
  
  ### Generate labels if these weren't specified
  if (is.null(xLabel.samplingDist)) {
    xLabel.samplingDist <- extractVarName(deparse(substitute(sampleVector)));
  }
  if (is.null(yLabel.samplingDist)) {
    yLabel.samplingDist <- paste0('Frequencies for ', res$samples, ' samples of n=', res$sampleSize);
  }

  ### Plot sampling distribution
  res$plot.samplingDist <- powerHist(tempDat$samplingDist,
                                     xLabel=xLabel.samplingDist,
                                     yLabel=yLabel.samplingDist,
                                     distributionColor=samplingDistColor,
                                     normalColor=normalColor,
                                     distributionLineSize=samplingDistLineSize,
                                     normalLineSize=normalLineSize)$plot +
    ggtitle("Sampling distribution");

  res$qqPlot.samplingDist <- ggqq(tempDat$samplingDist,
                                  sampleSizeOverride = res$sampleSize);

  ### Shapiro Wilk test - if there are more than 5000
  ### datapoints, only use the first 5000 datapoints
  res$sw.sampleDist <- ifelseObj(res$sampleSize > 5000,
                              shapiro.test(res$sampleVector[1:5000]),
                              shapiro.test(res$sampleVector));
  res$sw.samplingDist <- ifelseObj(res$samples > 5000,
                                shapiro.test(res$samplingDistribution[1:5000]),
                                shapiro.test(res$samplingDistribution));
  
  ### Anderson-Darling test
  res$ad.sampleDist <- ad.test_from_nortest(res$sampleVector);
  res$ad.samplingDist <- ad.test_from_nortest(res$samplingDistribution);
  
  ### Kolomogorov-Smirnof test
  suppressWarnings(res$ks.sampleDist <-
                     ks.test(res$sampleVector, "pnorm", alternative = "two.sided"));
  suppressWarnings(res$ks.samplingDist <-
                     ks.test(res$samplingDistribution, "pnorm", alternative = "two.sided"));

  ### Skewness and kurtosis
  res$dataShape.sampleDist <- dataShape(res$sampleVector, plots=FALSE);
  res$dataShape.samplingDist <- dataShape(res$samplingDistribution, sampleSizeOverride=ifelse(sampleSizeOverride,
                                                                                              length(res$sampleVector),
                                                                                              NULL),
                                          plots=FALSE);

  ### Set class for returnable object and return it
  class(res) <- 'normalityAssessment';
  return(res);
  
}

print.normalityAssessment <- function (x, ...) {

  if (x$sampleSize > 5000) {
    sw.sampleDist <- paste0("Shapiro-Wilk: p=", round(x$sw.sampleDist$p.value, x$digits),
                                   " (W=", round(x$sw.sampleDist$statistic, x$digits),
                                   "; NOTE: based on the first 5000 of ",
                                 x$sampleSize, " observations)");
  }
  else {
    sw.sampleDist <- paste0("Shapiro-Wilk: p=", round(x$sw.sampleDist$p.value, x$digits),
                                   " (W=", round(x$sw.sampleDist$statistic, x$digits),
                                   "; based on ", x$sampleSize, " observations)");
  }
  
  if (x$samples > 5000) {
    sw.samplingDist <- paste0("Shapiro-Wilk: p=", round(x$sw.samplingDist$p.value, x$digits),
                       " (W=", round(x$sw.samplingDist$statistic, x$digits),
                       "; NOTE: based on the first 5000 of ",
                       x$samples, " observations)");
  }
  else {
    sw.samplingDist <- paste0("Shapiro-Wilk: p=", round(x$sw.samplingDist$p.value, x$digits),
                           " (W=", round(x$sw.samplingDist$statistic, x$digits),
                           "; based on ", x$samples, " observations)");
  }
  
  ### Show output
  cat("## SAMPLE DISTRIBUTION ###\n");
  cat(paste0("Sample distribution of ", x$sampleSize,
             " observations\n",
             "Mean=", round(mean(x$sampleVector), x$digits),
             ", median=", round(median(x$sampleVector), x$digits),
             ", SD=", round(sd(x$sampleVector), x$digits),
             ", and therefore SE of the mean = ",
             round(sd(x$sampleVector)/sqrt(x$sampleSize), x$digits),
             "\n\n"));
  print(x$dataShape.sampleDist, extraNotification=FALSE);
  cat(paste0("\n", sw.sampleDist, "\n",
             "Anderson-Darling: p=",
             round(x$ad.sampleDist$p.value, x$digits),
             # round(x$ad.sampleDist@test$p.value, x$digits),
             " (A=",
             round(x$ad.sampleDist$statistic, x$digits),
             # round(x$ad.sampleDist@test$statistic, x$digits),
             ")\n",
             "Kolmogorov-Smirnof: p=", round(x$ks.sampleDist$p.value, x$digits),
             " (D=", round(x$ks.sampleDist$statistic, x$digits), ")"));
  
  cat("\n\n## SAMPLING DISTRIBUTION FOR THE MEAN ###\n");
  cat(paste0("Sampling distribution of ", x$samples, " samples of n=", x$sampleSize, "\n",
             "Mean=", round(mean(x$samplingDistribution), x$digits),
             ", median=", round(median(x$samplingDistribution), x$digits),
             ", SD=", round(sqrt(var(x$samplingDistribution)), x$digits),
             "\n\n"));
  print(x$dataShape.samplingDist, extraNotification=FALSE);
  cat(paste0("\n", sw.samplingDist, "\n",
             "Anderson-Darling: p=",
             round(x$ad.samplingDist$p.value, x$digits),
             # round(x$ad.samplingDist@test$p.value, x$digits),
             " (A=",
             round(x$ad.samplingDist$statistic, x$digits),
             # round(x$ad.samplingDist@test$statistic, x$digits),
             ")\n",
             "Kolmogorov-Smirnof: p=", round(x$ks.samplingDist$p.value, x$digits),
             " (D=", round(x$ks.samplingDist$statistic, x$digits), ")"));

  ### Plots
  grid.arrange(x$plot.sampleDist,
               x$plot.samplingDist,
               x$qqPlot.sampleDist,
               x$qqPlot.samplingDist,
               ncol=2);
  
  invisible(); 
}

pander.normalityAssessment <- function (x, headerPrefix = "#####",
                                        suppressPlot = FALSE, ...) {
  
  if (x$sampleSize > 5000) {
    sw.sampleDist <- paste0("Shapiro-Wilk: ", formatPvalue(x$sw.sampleDist$p.value, x$digits + 1),
                            " (W=", round(x$sw.sampleDist$statistic, x$digits),
                            "; NOTE: based on the first 5000 of ",
                            x$sampleSize, " observations)");
  }
  else {
    sw.sampleDist <- paste0("Shapiro-Wilk: ", formatPvalue(x$sw.sampleDist$p.value, x$digits + 1),
                            " (W=", round(x$sw.sampleDist$statistic, x$digits),
                            "; based on ", x$sampleSize, " observations)");
  }
  
  if (x$samples > 5000) {
    sw.samplingDist <- paste0("Shapiro-Wilk: ", formatPvalue(x$sw.samplingDist$p.value, x$digits + 1),
                              " (W=", round(x$sw.samplingDist$statistic, x$digits),
                              "; NOTE: based on the first 5000 of ",
                              x$samples, " observations)");
  }
  else {
    sw.samplingDist <- paste0("Shapiro-Wilk: ", formatPvalue(x$sw.samplingDist$p.value, x$digits + 1),
                              " (W=", round(x$sw.samplingDist$statistic, x$digits),
                              "; based on ", x$samples, " observations)");
  }
  
  ### Show output
  cat0("\n\n\n", headerPrefix, " Sample distribution\n\n");
  cat(paste0("Sample distribution of ", x$sampleSize,
             " observations  \n",
             "Mean=", round(mean(x$sampleVector), x$digits),
             ", median=", round(median(x$sampleVector), x$digits),
             ", SD=", round(sd(x$sampleVector), x$digits),
             ", and therefore SE of the mean = ",
             round(sd(x$sampleVector)/sqrt(x$sampleSize), x$digits),
             "\n\n"));
  pander(x$dataShape.sampleDist, extraNotification=FALSE);
  cat(paste0("\n\n", sw.sampleDist, "  \n",
             "Anderson-Darling: ",
             formatPvalue(x$ad.sampleDist$p.value,x$digits + 1),
             # formatPvalue(x$ad.sampleDist@test$p.value,x$digits + 1),
             " (A=",
             round(x$ad.sampleDist$statistic, x$digits),
             # round(x$ad.sampleDist@test$statistic, x$digits),
             ")  \n",
             "Kolmogorov-Smirnof: ", formatPvalue(x$ks.sampleDist$p.value, x$digits + 1),
             " (D=", round(x$ks.sampleDist$statistic, x$digits), ")"));
  
  cat0("\n\n", headerPrefix, " Sampling distribution of the mean\n\n");
  cat(paste0("Sampling distribution of ", x$samples, " samples of n=", x$sampleSize, "  \n",
             "Mean=", round(mean(x$samplingDistribution), x$digits),
             ", median=", round(median(x$samplingDistribution), x$digits),
             ", SD=", round(sqrt(var(x$samplingDistribution)), x$digits),
             "\n\n"));
  pander(x$dataShape.samplingDist, extraNotification=FALSE);
  cat(paste0("\n\n", sw.samplingDist, "  \n",
             "Anderson-Darling: ",
             formatPvalue(x$ad.samplingDist$p.value, x$digits + 1),
             # formatPvalue(x$ad.samplingDist@test$p.value, x$digits + 1),
             " (A=",
             round(x$ad.samplingDist$statistic, x$digits),
             # round(x$ad.samplingDist@test$statistic, x$digits),
             ")  \n",
             "Kolmogorov-Smirnof: ", formatPvalue(x$ks.samplingDist$p.value, x$digits + 1),
             " (D=", round(x$ks.samplingDist$statistic, x$digits), ")"));
  cat("\n\n\n");
  ### Plots
  if (!suppressPlot) {
    grid.arrange(x$plot.sampleDist,
                 x$plot.samplingDist,
                 x$qqPlot.sampleDist,
                 x$qqPlot.samplingDist,
                 ncol=2);
  }
  invisible(); 
}
