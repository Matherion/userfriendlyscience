\name{pwr.confIntR}
\alias{pwr.confIntR}
\title{
Determine required sample size for a given confidence interval width for Pearson's r
}
\description{
This function computes how many participants you need if you want to achieve a confidence interval of a given width. This is useful when you do a study and you are interested in how strongly two variables are associated.
}
\usage{
pwr.confIntR(r, w = 0.1, conf.level = 0.95)
}
\arguments{
  \item{r}{
  The correlation you expect to find (confidence intervals for a given level of confidence get narrower as the correlation coefficient increases).
}
  \item{w}{
  The required half-width (or margin of error) of the confidence interval.
}
  \item{conf.level}{
  The level of confidence.
}
}
\value{
The required sample size, or a vector or matrix of sample sizes if multiple correlation coefficients or required (half-)widths were supplied. The row and column names specify the \code{r} and \code{w} values to which the sample size in each cell corresponds. The confidence level is set as attribute to the resulting vector or matrix.
}
\references{
Bonett, D. G., Wright, T. A. (2000). Sample size requirements for estimating Pearson, Kendall and Spearman correlations. \emph{Psychometrika, 65}, 23-28.

Bonett, D. G. (2014). CIcorr.R and sizeCIcorr.R http://people.ucsc.edu/~dgbonett/psyc181.html

Moinester, M., & Gottfried, R. (2014). Sample size estimation for correlations with pre-specified confidence interval. \emph{The Quantitative Methods of Psychology, 10}(2), 124–130. http://www.tqmp.org/RegularArticles/vol10-2/p124/p124.pdf

Peters, G. J. Y. & Crutzen, R. (forthcoming) An easy and foolproof method for establishing how effective an intervention or behavior change method is: required sample size for accurate parameter estimation in health psychology.
}
\author{
Douglas Bonett (UC Santa Cruz, United States), with minor edits by Murray Moinester (Tel Aviv University, Israel) and Gjalt-Jorn Peters (Open University of the Netherlands, the Netherlands).

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{pwr.confIntR}}
}
\examples{
pwr.confIntR(c(.4, .6, .8), w=c(.1, .2));
}
\keyword{ htest }
