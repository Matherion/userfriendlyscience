\name{confIntR}
\alias{confIntR}
\title{
A function to compute a correlation's confidence interval
}
\description{
This function computes the confidence interval for a given correlation and its sample size. This is useful to obtain confidence intervals for correlations reported in papers when informing power analyses.
}
\usage{
confIntR(r, N, conf.level = 0.95)
}
\arguments{
  \item{r}{
  The observed correlation coefficient.
}
  \item{N}{
  The sample size of the sample where the correlation was computed.
}
  \item{conf.level}{
  The desired confidence level of the confidence interval.
}
}
\value{
The confidence interval(s) in a matrix with two columns. The left column contains the lower bound, the right column the upper bound. The \code{\link{rownames}} are the observed correlations, and the \code{\link{colnames}} are 'lo' and 'hi'. The confidence level and sample size are stored as attributes. The results are returned like this to make it easy to access single correlation coefficients from the resulting object (see the examples).

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
  
  ### To request confidence intervals for one correlation
  confIntR(.3, 100);
  
  ### The lower bound of a single correlation
  confIntR(.3, 100)[1];

  ### To request confidence intervals for multiple correlations:
  confIntR(c(.1, .3, .5), 250);
  
  ### The upper bound of the correlation of .5:
  confIntR(c(.1, .3, .5), 250)['0.5', 'hi'];

}
\keyword{ htest }
