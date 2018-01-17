\name{oneway}
\alias{oneway}
\title{
oneway
}
\description{
The oneway function wraps a number of analysis of variance functions into
one convenient interface that is similar to the oneway anova command in
SPSS.
}
\usage{
oneway(y, x, posthoc=NULL, means=FALSE, 
       fullDescribe=FALSE, levene=FALSE,
       plot=FALSE, digits=2, omegasq = TRUE,
       etasq = TRUE, corrections = FALSE,
       pvalueDigits=3, t=FALSE, conf.level=.95,
       posthocLetters = FALSE, posthocLetterAlpha = .05,
       silent=FALSE)
}
\arguments{
  \item{y}{
y has to be a numeric vector.
}
  \item{x}{
x has to be vector that either is a factor or can be converted into one.
}
  \item{posthoc}{
Which post-hoc tests to conduct. Valid values are any correction methods in
p.adjust.methods (at the time of writing of this document, "holm",
"hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"), as well as
"tukey" and "games-howell".
}
  \item{means}{
Whether to show the means for the y variable in each of the groups
determined by the x variable.
}
  \item{fullDescribe}{
  If TRUE, not only the means are shown, but all statistics acquired through
  the 'describe' function in the 'psych' package are shown.
}
  \item{levene}{
Whether to show Levene's test for equality of variances (using \code{\link{car}}'s \code{\link{leveneTest}} function but specifying \code{\link{mean}} as function to compute the center of each group).
}
  \item{plot}{
Whether to show a plot of the means of the y variable in each of the
groups determined by the x variable.
}
  \item{digits}{
The number of digits to show in the output.
}
  \item{omegasq}{
  Whether to show the omega squared effect size.
}
  \item{etasq}{
  Whether to show the eta squared effect size (this is biased and generally advised against; omega squared is less biased).
}
  \item{corrections}{
  Whether to show the corrections for unequal variances (Welch and Brown-Forsythe).
}
  \item{pvalueDigits}{
The number of digits to show for p-values; smaller p-values will be shown as
<.001 or <.0001 etc.
}
  \item{t}{
Whether to transpose the dataframes with the means (if requested) and the
anova results. This can be useful for blind people.
}
  \item{conf.level}{
Confidence level to use when computing the confidence interval for eta^2.
Note that the function we use doubles the 'unconfidence' level to maintain 
consistency with the NHST value (see http://yatani.jp/HCIstats/ANOVA#RCodeOneWay,
http://daniellakens.blogspot.nl/2014/06/calculating-confidence-intervals-for.html 
or Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals
and tests of close fit in the analysis of variance and contrast analysis.
Psychological methods, 9(2), 164-82. doi:10.1037/1082-989X.9.2.164
}
  \item{posthocLetters}{
  Whether to also compute and show the letters signifying differences between groups when conducting post hoc tests. This requires package \code{multcompView} to be installed.
}
  \item{posthocLetterAlpha}{
  The alpha to use when determining whether groups have different means when using \code{posthocLetters}.
}
  \item{silent}{
Whether to show warnings and other diagnostic information or remain silent.
}
}
\value{
A list of three elements:
\item{input}{List with input arguments}
\item{intermediate}{List of intermediate objects, such as the aov and
Anova (from the car package) objects.}
\item{output}{List with etasq, the effect size, and dat, a dataframe with
the Oneway Anova results.}
}
\note{
By my knowledge the Brown-Forsythe correction was not yet available in R. I took this from the original paper (directed there by Field, 2014). Note that this is the corrected \emph{F} value, not the Brown-Forsythe test for normality!
}
\references{
Brown, M., & Forsythe, A. (1974). \emph{The small sample behavior of some statistics which test the equality of several means.} Technometrics, 16(1), 129-132. https://doi.org/10.2307/1267501

Field, A. (2014) \emph{Discovering statistics using SPSS} (4th ed.). London: Sage.

Steiger, J. H. (2004). \emph{Beyond the F test: Effect size confidence intervals and tests of close fit in the analysis of variance and contrast analysis}. Psychological methods, 9(2), 164-82. doi:10.1037/1082-989X.9.2.164
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
### Do a oneway Anova
oneway(y=ChickWeight$weight, x=ChickWeight$Diet);

### Also order means and transpose the results
oneway(y=ChickWeight$weight, x=ChickWeight$Diet, means=TRUE, t=TRUE);
}
\keyword{ utilities }
