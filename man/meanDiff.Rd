\name{meanDiff}
\alias{meanDiff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
meanDiff
}
\description{
The meanDiff function compares the means between two groups. It computes Cohen's d, the unbiased estimate of Cohen's d (Hedges' g), and performs a t-test. It also shows the achieved power, and, more usefully, the power to detect small, medium, and large effects.
}
\usage{
meanDiff(x, y=NULL, paired = FALSE, r.prepost = NULL, var.equal = "test",
         conf.level = .95, plot = FALSE, digits = 2, envir = parent.frame())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  Dichotomous factor: variable 1; can also be a formula of the form y ~ x, where x must be a factor with two levels (i.e. dichotomous).
}
  \item{y}{
  Numeric vector: variable 2; can be empty if x is a formula.
}
  \item{paired}{
  Boolean; are x & y independent or dependent? Note that if x & y are dependent, they need to have the same length.
}
  \item{r.prepost}{
  Correlation between the pre- and post-test in the case of a paired samples
  t-test. This is required to compute Cohen's d using the formula on page
  29 of Borenstein et al. (2009). If NULL, the correlation is simply
  computed from the provided scores (but of course it will then be lower
  if these is an effect - this will lead to an underestimate of the
  within-groups variance, and therefore, of the standard error of Cohen's d,
  and therefore, to confidence intervals that are too narrow (too liberal).
  Also, of course, when using this data to compute the within-groups
  correlation, random variations will also impact that correlation, which
  means that confidence intervals may in practice deviate from the null
  hypothesis significance testing p-value in either direction (i.e. the
  p-value may indicate a significant association while the confidence
  interval contains 0, or the other way around). Therefore, if the
  test-retest correlation of the relevant measure is known, please provide
  this here to enable computation of accurate confidence intervals.
}
  \item{var.equal}{
  String; only relevant if x & y are independent; can be "test" (default; test whether x & y have different variances), "no" (assume x & y have different variances; see the Warning below!), or "yes" (assume x & y have the same variance)
}
  \item{conf.level}{
  Confidence of confidence intervals you want.
}
  \item{plot}{
  Whether to print a dlvPlot.
}
  \item{digits}{
  With what precision you want the results to print.
}
  \item{envir}{
  The environment where to search for the variables (useful when calling meanDiff from a function where the vectors are defined in that functions environment).
}
}
\details{
This function uses the formulae from Borenstein, Hedges, Higgins & Rothstein (2009) (pages 25-32).
}
\value{
An object is returned with the following elements:
  \item{variables}{Input variables}
  \item{groups}{Levels of the x variable, the dichotomous factor}
  \item{ci.confidence}{Confidence of confidence intervals}
  \item{digits}{Number of digits for output}
  \item{x}{Values of dependent variable in first group}
  \item{y}{Values of dependent variable in second group}
  \item{type}{Type of t-test (independent or dependent, equal variances or not)}
  \item{n}{Sample sizes of the two groups}
  \item{mean}{Means of the two groups}
  \item{sd}{Standard deviations of the two groups}
  \item{objects}{Objects used; the t-test and optionally the test for equal variances}
  \item{variance}{Variance of the difference score}
  \item{meanDiff}{Difference between the means}
  \item{meanDiff.d}{Cohen's d}
  \item{meanDiff.d.var}{Variance of Cohen's d}
  \item{meanDiff.d.se}{Standard error of Cohen's d}
  \item{meanDiff.J}{Correction for Cohen's d to get to the unbiased Hedges g}
  \item{power}{Achieved power with current effect size and sample size}
  \item{power.small}{Power to detect small effects with current sample size}
  \item{power.medium}{Power to detect medium effects with current sample size}
  \item{power.largel}{Power to detect large effects with current sample size}
  \item{meanDiff.g}{Hedges' g}
  \item{meanDiff.g.var}{Variance of Hedges' g}
  \item{meanDiff.g.se}{Standard error of Hedges' g}
  \item{ci.usedZ}{Z value used to compute confidence intervals}
  \item{meanDiff.d.ci.lower}{Lower bound of confidence interval around Cohen's d}
  \item{meanDiff.d.ci.upper}{Upper bound of confidence interval around Cohen's d}
  \item{meanDiff.g.ci.lower}{Lower bound of confidence interval around Hedges' g}
  \item{meanDiff.g.ci.upper}{Upper bound of confidence interval around Hedges' g}
  \item{meanDiff.ci.lower}{Lower bound of confidence interval around raw mean}
  \item{meanDiff.ci.upper}{Upper bound of confidence interval around raw mean}
  \item{t}{Student t value for Null Hypothesis Significance Testing}
  \item{df}{Degrees of freedom for t value}
  \item{p}{p-value corresponding to t value}
}
\references{
Borenstein, M., Hedges, L. V., Higgins, J. P., & Rothstein, H. R. (2011). Introduction to meta-analysis. John Wiley & Sons.
}
\section{Warning}{
  Note that when different variances are assumed for the t-test (i.e. the null-hypothesis test), the values of Cohen's d are still based on the assumption that the variance is equal. In this case, the confidence interval might, for example, not contain zero even though the NHST has a non-significant p-value (the reverse can probably happen, too).
}
\examples{
### Create simple dataset
dat <- PlantGrowth[1:20,];
### Remove third level from group factor
dat$group <- factor(dat$group);
### Compute mean difference and show it
meanDiff(dat$weight ~ dat$group);

### Look at second treatment
dat <- rbind(PlantGrowth[1:10,], PlantGrowth[21:30,]);
### Remove third level from group factor
dat$group <- factor(dat$group);
### Compute mean difference and show it
meanDiff(x=dat$group, y=dat$weight);
}
\keyword{ utilities }