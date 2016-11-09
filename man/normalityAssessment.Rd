\name{normalityAssessment}
\alias{normalityAssessment}
\alias{samplingDistribution}
\alias{dataShape}
\title{
normalityAssessment and samplingDistribution
}
\description{
normalityAssessment can be used to assess whether a variable and the sampling distribution of its mean have an approximately normal distribution.

samplingDistribution is a convenient wrapper for normalityAssessment that makes it easy to quickly generate a sample and sampling distribution from frequencies (or proportions).

dataShape computes the skewness and kurtosis.
}
\usage{
normalityAssessment(sampleVector, samples = 10000, digits=2,
                    samplingDistColor = "#2222CC",
                    normalColor = "#00CC00",
                    samplingDistLineSize = 2,
                    normalLineSize = 1,
                    xLabel.sampleDist = NULL,
                    yLabel.sampleDist = NULL,
                    xLabel.samplingDist = NULL,
                    yLabel.samplingDist = NULL,
                    sampleSizeOverride = TRUE)
samplingDistribution(popValues = c(0, 1),
                     popFrequencies = c(50, 50),
                     sampleSize = NULL,
                     sampleFromPop = FALSE, ...)
dataShape(sampleVector, na.rm = TRUE, type = 2,
          digits = 2, conf.level = 0.95,
          plots = TRUE, xLabs = NA,
          yLabs = NA, qqCI = TRUE,
          labelOutliers = TRUE,
          sampleSizeOverride = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sampleVector}{
  Numeric vector containing the sample data.
}
  \item{samples}{
  Number of samples to use when constructing sampling distribution.
}
  \item{digits}{
  Number of digits to use when printing results.
}
  \item{samplingDistColor}{
  Color to use when drawing the sampling distribution.
}
  \item{normalColor}{
  Color to use when drawing the standard normal curve.
}
  \item{samplingDistLineSize}{
  Size of the line used to draw the sampling distribution.
}
  \item{normalLineSize}{
  Size of the line used to draw the standard normal distribution.
}
  \item{xLabel.sampleDist}{
  Label of x axis of the distribution of the sample.
}
  \item{yLabel.sampleDist}{
  Label of y axis of the distribution of the sample.
}
  \item{xLabel.samplingDist}{
  Label of x axis of the sampling distribution.
}
  \item{yLabel.samplingDist}{
  Label of y axis of the sampling distribution.
}
  \item{xLabs, yLabs}{
  The axis labels for the three plots (should be vectors of three elements; the first specifies the X or Y axis label for the rightmost plot (the histogram), the second for the middle plot (the QQ plot), and the third for the rightmost plot (the box plot).
  }
  \item{popValues}{
  The possible values (levels) of the relevant variable. For example, for a dichotomous variable, this can be "c(1:2)" (or "c(1, 2)"). Note that samplingDistribution is for manually specifying the frequency distribution (or proportions); if you have a vector with 'raw' data, just call normalityAssessment directly.
}
  \item{popFrequencies}{
  The frequencies corresponding to each value in popValues; must be in the same order! See the examples.
}
  \item{sampleSize}{
  Size of the sample; the sum of the frequencies if not specified.
}
  \item{na.rm}{
  Whether to remove missing data first.
}
  \item{type}{
  Type of skewness and kurtosis to compute; either 1 (g1 and g2), 2 (G1 and G2), or 3 (b1 and b2). See Joanes & Gill (1998) for more information.
}
  \item{conf.level}{
  Confidence of confidence intervals.
}
  \item{plots}{
  Whether to display plots.
  }
    \item{qqCI}{
  Whether to show the confidence interval for the QQ plot.
  }
  \item{labelOutliers}{
  Whether to label outliers with their row number in the box plot.
  }
  \item{sampleFromPop}{
  If true, the sample vector is created by sampling from the population information specified;
  if false, rep() is used to generate the sample vector. Note that is proportions are
  supplied in popFrequencies, sampling from the population is necessary!
}
  \item{sampleSizeOverride}{
  Whether to use the sample size of the sample as sample size for the sampling distribution,
  instead of the sampling distribution size. This makes sense, because otherwise, the sample
  size and thus sensitivity of the null hypothesis significance tests is a function of the
  number of samples used to generate the sampling distribution.
  }
  \item{...}{
  Anything else is passed on my sampingDistribution to normalityAssessment.
}
}
\details{

normalityAssessment provides a number of normality tests and draws histograms
of the sample data and the sampling distribution of the mean (most
statistical tests assume the latter is normal, rather than the first;
normality of the sample data guarantees normality of the sampling
distribution of the mean, but if the sample size is sufficiently large,
the sampling distribution of the mean is approximately normal even when
the sample data are not normally distributed). Note that for the sampling
distribution, the degrees of freedom are usually so huge that the normality
tests, negligible deviations from normality will already result in
very small p-values.

samplingDistribution makes it easy to quickly assess the distribution of a variables
based on frequencies or proportions, and dataShape computes skewness and kurtosis.

}
\value{

An object with several results, the most notably of which are:
  \item{plot.sampleDist}{Histogram of sample distribution}
  \item{sw.sampleDist}{Shapiro-Wilk normality test of sample distribution}
  \item{ad.sampleDist}{Anderson-Darling normality test of sample distribution}
  \item{ks.sampleDist}{Kolmogorov-Smirnof normality test of sample distribution}
  \item{kurtosis.sampleDist}{Kurtosis for sample distribution}
  \item{skewness.sampleDist}{Skewness for sample distribution}
  \item{plot.samplingDist}{Histogram of sampling distribution}
  \item{sw.samplingDist}{Shapiro-Wilk normality test of sampling distribution}
  \item{ad.samplingDist}{Anderson-Darling normality test of sampling distribution}
  \item{ks.samplingDist}{Kolmogorov-Smirnof normality test of sampling distribution}
  \item{dataShape.samplingDist}{Skewness and kurtosis for sampling distribution}
}
\examples{
### Note: the 'not run' is simply because running takes a lot of time,
###       but these examples are all safe to run!
\dontrun{

normalityAssessment(rnorm(35));

### Create a distribution of three possible values and
### show the sampling distribution for the mean
popValues <- c(1, 2, 3);
popFrequencies <- c(20, 50, 30);
sampleSize <- 100;
samplingDistribution(popValues = popValues,
                     popFrequencies = popFrequencies,
                     sampleSize = sampleSize);

### Create a very skewed distribution of ten possible values
popValues <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
popFrequencies <- c(2, 4, 8, 6, 10, 15, 12, 200, 350, 400);
samplingDistribution(popValues = popValues,
                     popFrequencies = popFrequencies,
                     sampleSize = sampleSize, digits=5);
}
}
\keyword{ utilities }
