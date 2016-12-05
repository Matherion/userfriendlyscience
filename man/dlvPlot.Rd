\name{dlvPlot}
\alias{dlvPlot}
\alias{dlvTheme}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dlvPlot
}
\description{
The dlvPlot function produces a dot-violin-line plot, and dlvTheme is the default theme.
}
\usage{
dlvPlot(dat, x = NULL, y, z = NULL, conf.level = .95,
        jitter = "FALSE", binnedDots = TRUE, binwidth=NULL,
        error="lines", dotsize="density", densityDotBaseSize=3,
        normalDotBaseSize=1, violinAlpha = .2, dotAlpha = .4,
        lineAlpha = 1, connectingLineAlpha = 1,
        meanDotSize=5, posDodge=0.2, errorType = "both")
dlvTheme(base_size = 11, base_family = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
  The dataframe containing x, y and z.
}
  \item{x}{
  Character value with the name of the predictor ('independent') variable, must refer to a categorical variable (i.e. a factor).
}
  \item{y}{
  Character value with the name of the critetion ('dependent') variable, must refer to a continuous variable (i.e. a numeric vector).
}
  \item{z}{
  Character value with the name of the moderator variable, must refer to a categorical variable (i.e. a factor).
}
  \item{conf.level}{
  Confidence of confidence intervals.
}
  \item{jitter}{
  Logical value (i.e. TRUE or FALSE) whether or not to jitter individual datapoints. Note that jitter cannot be combined with posDodge (see below).
}
  \item{binnedDots}{
  Logical value indicating whether to use binning to display the dots. Overrides jitter and dotsize.
} 
  \item{binwidth}{
  Numeric value indicating how broadly to bin (larger values is more binning, i.e. combining more dots into one big dot).
} 
  \item{error}{
  Character value: "none", "lines" or "whiskers"; indicates whether to show the confidence interval as lines with (whiskers) or without (lines) horizontal whiskers or not at all (none)
}
  \item{dotsize}{
  Character value: "density" or "normal"; when "density", the size of each dot corresponds to the density of the distribution at that point.
}
  \item{densityDotBaseSize}{
  Numeric value indicating base size of dots when their size corresponds to the density (bigger = larger dots).
}
  \item{normalDotBaseSize}{
  Numeric value indicating base size of dots when their size is fixed (bigger = larger dots).
}
  \item{violinAlpha}{
  Numeric value indicating alpha value of violin layer (0 = completely transparent, 1 = completely opaque).
}
  \item{dotAlpha}{
  Numeric value indicating alpha value of dot layer (0 = completely transparent, 1 = completely opaque).
}
  \item{lineAlpha}{
  Numeric value indicating alpha value of the confidence interval line layer (0 = completely transparent, 1 = completely opaque).
}
  \item{connectingLineAlpha}{
  Numeric value indicating alpha value of the layer with the lines connecting the means (0 = completely transparent, 1 = completely opaque).
}
  \item{meanDotSize}{
  Numeric value indicating the size of the dot used to indicate the mean in the line layer.
}
  \item{posDodge}{
  Numeric value indicating the distance to dodge positions (0 for complete overlap).
}
  \item{errorType}{
  If the error is shown using lines, this argument indicates Whether the
  errorbars should show the confidence interval (\code{errorType='ci'}),
  the standard errors (\code{errorType='se'}), or both
  (\code{errorType='both'}). In this last case, the standard error will be
  wider than the confidence interval.
}
  \item{base_size, base_family, ...}{
  Passed on to the ggplot theme_grey() function.
}
}
\details{
This function creates Dot Violin Line plots. One image says more than a thousand words; I suggest you run the example :-)
}
\value{
The behavior of this function depends on the arguments.

If no x and z are provided and y is a character value, dlvPlot produces a univariate plot for the numerical y variable.

If no x and z are provided, and y is c character vector, dlvPlot produces multiple Univariate plots, with variable names determining categories on x-axis and with numerical y variables on y-axis

If both x and y are a character value, and no z is provided, dlvPlot produces a bivariate plot where factor x determines categories on x-axis with numerical variable y on the y-axis (roughly a line plot with a single line)

Finally, if x, y and z are each a character value, dlvPlot produces multivariate plot where factor x determines categories on x-axis, factor z determines the different lines, and with the numerical y variable on the y-axis

An object is returned with the following elements:
  \item{dat.raw}{Raw datafile provided when calling dlvPlot}
  \item{dat}{Transformed (long) datafile dlvPlot uses}
  \item{descr}{Dataframe with extracted descriptives used to plot the mean and confidence intervals}
  \item{yRange}{The range of the Y variable used to construct the plot}
  \item{plot}{The plot itself}
}
\examples{
### Note: the 'not run' is simply because running takes a lot of time,
###       but these examples are all safe to run!
\dontrun{
### Create simple dataset
dat <- data.frame(x1 = factor(rep(c(0,1), 20)),
                  x2 = factor(c(rep(0, 20), rep(1, 20))),
                  y=rep(c(4,5), 20) + rnorm(40));
### Generate a simple dlvPlot of y
dlvPlot(dat, y='y');
### Now add a predictor
dlvPlot(dat, x='x1', y='y');
### And finally also a moderator:
dlvPlot(dat, x='x1', y='y', z='x2');
### The number of datapoints might be a bit clearer if we jitter
dlvPlot(dat, x='x1', y='y', z='x2', jitter=TRUE);
### Although just dodging the density-sized dots might work better
dlvPlot(dat, x='x1', y='y', z='x2', posDodge=.3);
}
}
\keyword{ utilities }
