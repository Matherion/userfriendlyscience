#' normalityAssessment and samplingDistribution
#' 
#' normalityAssessment can be used to assess whether a variable and the
#' sampling distribution of its mean have an approximately normal distribution.
#' 
#' samplingDistribution is a convenient wrapper for normalityAssessment that
#' makes it easy to quickly generate a sample and sampling distribution from
#' frequencies (or proportions).
#' 
#' dataShape computes the skewness and kurtosis.
#' 
#' 
#' normalityAssessment provides a number of normality tests and draws
#' histograms of the sample data and the sampling distribution of the mean
#' (most statistical tests assume the latter is normal, rather than the first;
#' normality of the sample data guarantees normality of the sampling
#' distribution of the mean, but if the sample size is sufficiently large, the
#' sampling distribution of the mean is approximately normal even when the
#' sample data are not normally distributed). Note that for the sampling
#' distribution, the degrees of freedom are usually so huge that the normality
#' tests, negligible deviations from normality will already result in very
#' small p-values.
#' 
#' samplingDistribution makes it easy to quickly assess the distribution of a
#' variables based on frequencies or proportions, and dataShape computes
#' skewness and kurtosis.
#' 
#' @aliases normalityAssessment samplingDistribution dataShape
#' @param sampleVector Numeric vector containing the sample data.
#' @param samples Number of samples to use when constructing sampling
#' distribution.
#' @param digits Number of digits to use when printing results.
#' @param samplingDistColor Color to use when drawing the sampling
#' distribution.
#' @param normalColor Color to use when drawing the standard normal curve.
#' @param samplingDistLineSize Size of the line used to draw the sampling
#' distribution.
#' @param normalLineSize Size of the line used to draw the standard normal
#' distribution.
#' @param xLabel.sampleDist Label of x axis of the distribution of the sample.
#' @param yLabel.sampleDist Label of y axis of the distribution of the sample.
#' @param xLabel.samplingDist Label of x axis of the sampling distribution.
#' @param yLabel.samplingDist Label of y axis of the sampling distribution.
#' @param xLabs,yLabs The axis labels for the three plots (should be vectors of
#' three elements; the first specifies the X or Y axis label for the rightmost
#' plot (the histogram), the second for the middle plot (the QQ plot), and the
#' third for the rightmost plot (the box plot).
#' @param popValues The possible values (levels) of the relevant variable. For
#' example, for a dichotomous variable, this can be "c(1:2)" (or "c(1, 2)").
#' Note that samplingDistribution is for manually specifying the frequency
#' distribution (or proportions); if you have a vector with 'raw' data, just
#' call normalityAssessment directly.
#' @param popFrequencies The frequencies corresponding to each value in
#' popValues; must be in the same order! See the examples.
#' @param sampleSize Size of the sample; the sum of the frequencies if not
#' specified.
#' @param na.rm Whether to remove missing data first.
#' @param type Type of skewness and kurtosis to compute; either 1 (g1 and g2),
#' 2 (G1 and G2), or 3 (b1 and b2). See Joanes & Gill (1998) for more
#' information.
#' @param conf.level Confidence of confidence intervals.
#' @param plots Whether to display plots.
#' @param qqCI Whether to show the confidence interval for the QQ plot.
#' @param labelOutliers Whether to label outliers with their row number in the
#' box plot.
#' @param sampleFromPop If true, the sample vector is created by sampling from
#' the population information specified; if false, rep() is used to generate
#' the sample vector. Note that is proportions are supplied in popFrequencies,
#' sampling from the population is necessary!
#' @param sampleSizeOverride Whether to use the sample size of the sample as
#' sample size for the sampling distribution, instead of the sampling
#' distribution size. This makes sense, because otherwise, the sample size and
#' thus sensitivity of the null hypothesis significance tests is a function of
#' the number of samples used to generate the sampling distribution.
#' @param ...  Anything else is passed on my sampingDistribution to
#' normalityAssessment.
#' @return
#' 
#' An object with several results, the most notably of which are:
#' \item{plot.sampleDist}{Histogram of sample distribution}
#' \item{sw.sampleDist}{Shapiro-Wilk normality test of sample distribution}
#' \item{ad.sampleDist}{Anderson-Darling normality test of sample distribution}
#' \item{ks.sampleDist}{Kolmogorov-Smirnof normality test of sample
#' distribution} \item{kurtosis.sampleDist}{Kurtosis for sample distribution}
#' \item{skewness.sampleDist}{Skewness for sample distribution}
#' \item{plot.samplingDist}{Histogram of sampling distribution}
#' \item{sw.samplingDist}{Shapiro-Wilk normality test of sampling distribution}
#' \item{ad.samplingDist}{Anderson-Darling normality test of sampling
#' distribution} \item{ks.samplingDist}{Kolmogorov-Smirnof normality test of
#' sampling distribution} \item{dataShape.samplingDist}{Skewness and kurtosis
#' for sampling distribution}
#' @keywords utilities
#' @examples
#' 
#' ### Note: the 'not run' is simply because running takes a lot of time,
#' ###       but these examples are all safe to run!
#' \dontrun{
#' 
#' normalityAssessment(rnorm(35));
#' 
#' ### Create a distribution of three possible values and
#' ### show the sampling distribution for the mean
#' popValues <- c(1, 2, 3);
#' popFrequencies <- c(20, 50, 30);
#' sampleSize <- 100;
#' samplingDistribution(popValues = popValues,
#'                      popFrequencies = popFrequencies,
#'                      sampleSize = sampleSize);
#' 
#' ### Create a very skewed distribution of ten possible values
#' popValues <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
#' popFrequencies <- c(2, 4, 8, 6, 10, 15, 12, 200, 350, 400);
#' samplingDistribution(popValues = popValues,
#'                      popFrequencies = popFrequencies,
#'                      sampleSize = sampleSize, digits=5);
#' }
#' 
#' @export normalityAssessment
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
