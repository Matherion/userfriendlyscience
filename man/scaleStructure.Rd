\name{scaleStructure}
\alias{scaleStructure}
\alias{scaleReliability}
\title{
scaleStructure
}
\description{
The scaleStructure function (which was originally called scaleReliability) computes a number of measures to assess scale reliability and internal consistency.

If you use this function in an academic paper, please cite Peters (2014), where the function is introduced, and/or Crutzen & Peters (2015), where the function is discussed from a broader perspective.
}
\usage{
scaleStructure(dat=NULL, items = 'all', digits = 2, ci = TRUE,
               interval.type="normal-theory", conf.level=.95,
               silent=FALSE, samples=1000, bootstrapSeed = NULL,
               omega.psych = TRUE, poly = TRUE)
scaleReliability(dat=NULL, items = 'all', digits = 2, ci = TRUE,
                 interval.type="normal-theory", conf.level=.95,
                 silent=FALSE, samples=1000, bootstrapSeed = NULL,
                 omega.psych = TRUE, poly = TRUE)
}
\arguments{
\item{dat}{
  A dataframe containing the items in the scale. All variables in this
  dataframe will be used if items = 'all'. If \code{dat} is \code{NULL},
  a the \code{\link{getData}} function will be called to show the user
  a dialog to open a file.
}
\item{items}{
  If not 'all', this should be a character vector with the names of the
  variables in the dataframe that represent items in the scale.
}
\item{digits}{
  Number of digits to use in the presentation of the results.
}
\item{ci}{
  Whether to compute confidence intervals as well. If true, the method
  specified in \code{interval.type} is used. When specifying a
  bootstrapping method, this can take quite a while!
}
\item{interval.type}{
  Method to use when computing confidence intervals. The list of methods
  is explained in \code{\link{ci.reliability}}. Note that when specifying
  a bootstrapping method, the method will be set to \code{normal-theory}
  for computing the confidence intervals for the ordinal estimates, because
  these are based on the polychoric correlation matrix, and raw data is
  required for bootstrapping.
}
\item{conf.level}{
  The confidence of the confidence intervals.
}
\item{silent}{
  If computing confidence intervals, the user is warned that it may take a
  while, unless \code{silent=TRUE}.
}
\item{samples}{
  The number of samples to compute for the bootstrapping of the confidence
  intervals.
}
\item{bootstrapSeed}{
  The seed to use for the bootstrapping - setting this seed makes it
  possible to replicate the exact same intervals, which is useful for
  publications.
}
\item{omega.psych}{
  Whether to also compute the interval estimate for omega using the
  \code{\link{omega}} function in the \code{\link{psych}} package.
  The default point estimate and confidence interval for omega are based
  on the procedure suggested by Dunn, Baguley & Brunsden (2013) using the
  \code{\link{MBESS}} function \code{\link{ci.reliability}} (because it
  has more options for computing confidence intervals, not always requiring
  bootstrapping), whereas the \code{\link{psych}} package point estimate was
  suggested in Revelle & Zinbarg (2008). The \code{\link{psych}} estimate
  usually (perhaps always) results in higher estimates for omega.
}
\item{poly}{
  Whether to compute ordinal measures (if the items have sufficiently
  few categories).
}
}
\details{

This function is basically a wrapper for functions from the psych and MBESS
packages that compute measures of reliability and internal consistency. For
backwards compatibility, in addition to \code{scaleStructure},
\code{scaleReliability} can also be used to call this function.

}
\value{

An object with the input and several output variables. Most notably:
  \item{input}{Input specified when calling the function}
  \item{intermediate}{Intermediate values and objects computed to get to the final results}
  \item{output}{Values of reliability / internal consistency measures, with as most notable elements:}
  \item{output$dat}{A dataframe with the most important outcomes}
  \item{output$omega}{Point estimate for omega}
  \item{output$glb}{Point estimate for the Greatest Lower Bound}
  \item{output$alpha}{Point estimate for Cronbach's alpha}
  \item{output$coefficientH}{Coefficient H}
  \item{output$omega.ci}{Confidence interval for omega}
  \item{output$alpha.ci}{Confidence interval for Cronbach's alpha}
}
\seealso{
  \code{\link{omega}}, \code{\link{alpha}}, and \code{\link{ci.reliability}}.
}
\references{
Crutzen, R., & Peters, G.-J. Y. (2015). Scale quality: alpha is an inadequate estimate and factor-analytic evidence is needed first of all. \emph{Health Psychology Review.} http://dx.doi.org/10.1080/17437199.2015.1124240

Dunn, T. J., Baguley, T., & Brunsden, V. (2014). From alpha to omega: A practical solution to the pervasive problem of internal consistency estimation. \emph{British Journal of Psychology}, 105(3), 399-412. doi:10.1111/bjop.12046

Eisinga, R., Grotenhuis, M. Te, & Pelzer, B. (2013). The reliability of a two-item scale: Pearson, Cronbach, or Spearman-Brown? \emph{International Journal of Public Health}, 58(4), 637-42. doi:10.1007/s00038-012-0416-3

Gadermann, A. M., Guhn, M., Zumbo, B. D., & Columbia, B. (2012). Estimating ordinal reliability for Likert-type and ordinal item response data: A conceptual, empirical, and practical guide. \emph{Practical Assessment, Research & Evaluation}, 17(3), 1-12.

Peters, G.-J. Y. (2014). The alpha and the omega of scale reliability and validity: why and how to abandon Cronbach's alpha and the route towards more comprehensive assessment of scale quality. \emph{European Health Psychologist}, 16(2), 56-69. \url{http://ehps.net/ehp/index.php/contents/article/download/ehp.v16.i2.p56/1}

Revelle, W., & Zinbarg, R. E. (2009). Coefficients Alpha, Beta, Omega, and the glb: Comments on Sijtsma. \emph{Psychometrika}, 74(1), 145-154. doi:10.1007/s11336-008-9102-z

Sijtsma, K. (2009). On the Use, the Misuse, and the Very Limited Usefulness of Cronbach's Alpha. \emph{Psychometrika}, 74(1), 107-120. doi:10.1007/s11336-008-9101-0

Zinbarg, R. E., Revelle, W., Yovel, I., & Li, W. (2005). Cronbach's alpha, Revelle's beta and McDonald's omega H: Their relations with each other and two alternative conceptualizations of reliability. \emph{Psychometrika}, 70(1), 123-133. doi:10.1007/s11336-003-0974-7
}
\author{
Gjalt-Jorn Peters and Daniel McNeish (University of North Carolina, Chapel Hill, US).

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{

\dontrun{
### (These examples take a lot of time, so they are not run
###  during testing.)

### This will prompt the user to select an SPSS file
scaleStructure();

### Load data from simulated dataset testRetestSimData (which
### satisfies essential tau-equivalence).
data(testRetestSimData);

### Select some items in the first measurement
exampleData <- testRetestSimData[2:6];

### Use all items (don't order confidence intervals to save time
### during automated testing of the example)
scaleStructure(dat=exampleData, ci=FALSE);

### Use a selection of three variables (without confidence
### intervals to save time
scaleStructure(dat=exampleData, items=c('t0_item2', 't0_item3', 't0_item4'),
               ci=FALSE);

### Make the items resemble an ordered categorical (ordinal) scale
ordinalExampleData <- data.frame(apply(exampleData, 2, cut,
                                       breaks=5, ordered_result=TRUE,
                                       labels=as.character(1:5)));

### Now we also get estimates assuming the ordinal measurement level
scaleStructure(ordinalExampleData, ci=FALSE);
}

}
\keyword{ utilities }
\keyword{ univar }
