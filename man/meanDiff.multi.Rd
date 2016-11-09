\name{meanDiff.multi}
\alias{meanDiff.multi}
\title{
meanDiff.multi
}
\description{
The meanDiff.multi function compares many means for many groups. It presents the results in a dataframe summarizing all relevant information, and produces plot showing the confidence intervals for the effect sizes for each predictor (i.e. dichotomous variable). Like meanDiff, it computes Cohen's d, the unbiased estimate of Cohen's d (Hedges' g), and performs a t-test. It also shows the achieved power, and, more usefully, the power to detect small, medium, and large effects.
}
\usage{
meanDiff.multi(dat, y, x=NULL, var.equal = "yes", conf.level = .95,
               digits = 2, orientation = "vertical",
               zeroLineColor = "grey", zeroLineSize = 1.2,
               envir = parent.frame())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
  The dataframe containing the variables involved in the mean tests.
}
  \item{y}{
  Character vector containing the list of interval variables to include in the tests.
}
  \item{x}{
  Character vector containing the list of the dichotomous variables to include in the tests. If x is empty, paired samples t-tests will be conducted.
}
  \item{var.equal}{
  String; only relevant if x & y are independent; can be "test" (default; test whether x & y have different variances), "no" (assume x & y have different variances; see the Warning below!), or "yes" (assume x & y have the same variance)
}
  \item{conf.level}{
  Confidence of confidence intervals you want.
}
  \item{digits}{
  With what precision you want the results to print.
}
  \item{orientation}{
  Whether to plot the effect size confidence intervals vertically (like a forest plot, the default) or horizontally.
}
  \item{zeroLineColor}{
  Color of the horizontal line at an effect size of 0 (set to 'white' to not display the line; also adjust the size to 0 then).
}
  \item{zeroLineSize}{
  Size of the horizontal line at an effect size of 0 (set to 0 to not display the line; also adjust the color to 'white' then).
}
  \item{envir}{
  The environment where to search for the variables (useful when calling meanDiff from a function where the vectors are defined in that functions environment).
}
}
\details{
This function uses the meanDiff function, which uses the formulae from Borenstein, Hedges, Higgins & Rothstein (2009) (pages 25-32).
}
\value{
An object is returned with the following elements:
  \item{results.raw}{Objects returned by the calls to meanDiff.}
  \item{plots}{For every comparison, a plot with the datapoints, means, and confidence intervals in the two groups.}
  \item{results.compiled}{Dataframe with the most important results from each comparison.}
  \item{plots.compiled}{For every dichotomous (x) variable, a plot with the confidence interval for the effect size of each dependent (y) variable.}
  \item{input}{The arguments with which the function was called.}
}
\references{
Borenstein, M., Hedges, L. V., Higgins, J. P., & Rothstein, H. R. (2011). Introduction to meta-analysis. John Wiley & Sons.
}
\section{Warning}{
  Note that when different variances are assumed for the t-test (i.e. the null-hypothesis test), the values of Cohen's d are still based on the assumption that the variance is equal. In this case, the confidence interval might, for example, not contain zero even though the NHST has a non-significant p-value (the reverse can probably happen, too).
}
\examples{
### Create simple dataset
dat <- data.frame(x1 = factor(rep(c(0,1), 20)),
                  x2 = factor(c(rep(0, 20), rep(1, 20))),
                  y=rep(c(4,5), 20) + rnorm(40));
### Compute mean difference and show it
meanDiff.multi(dat, x=c('x1', 'x2'), y='y', var.equal="yes");
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }