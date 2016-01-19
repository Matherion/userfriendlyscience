\name{associationMatrix Helper Functions}
\alias{computeStatistic_t}
\alias{computeStatistic_r}
\alias{computeStatistic_f}
\alias{computeStatistic_chisq}
\alias{computeEffectSize_d}
\alias{computeEffectSize_r}
\alias{computeEffectSize_etasq}
\alias{computeEffectSize_v}
\alias{associationMatrixESDefaults}
\alias{associationMatrixStatDefaults}
\title{
associationMatrix Helper Functions
}
\description{
These objects contain a number of settings and functions for associationMatrix.
}
\usage{
computeStatistic_t(var1, var2, conf.level=.95)
computeStatistic_r(var1, var2, conf.level=.95)
computeStatistic_f(var1, var2, conf.level=.95)
computeStatistic_chisq(var1, var2, conf.level=.95)

computeEffectSize_d(var1, var2, conf.level=.95)
computeEffectSize_r(var1, var2, conf.level=.95)
computeEffectSize_etasq(var1, var2, conf.level=.95)
computeEffectSize_v(var1, var2, conf.level=.95,
                         bootstrap=FALSE, samples=5000)
}
\arguments{
  \item{var1}{
  One of the two variables for which to compute a statistic or effect size
}
  \item{var2}{
  The other variable for which to compute the statistic or effect size
}
  \item{conf.level}{
  The confidence for the confidence interval for the effect size
}
  \item{bootstrap}{
  Whether to bootstrap to estimate the confidence interval for Cramer's V.
  If FALSE, the Fisher's Z conversion is used.
}
  \item{samples}{
  If bootstrapping, the number of samples to generate (of course, more
  samples means more accuracy and longer processing time).
}
}
\value{

  associationMatrixStatDefaults and associationMatrixESDefaults contain
  the default functions from computeStatistic and computeEffectSize that
  are called (see the help file for associationMatrix for more details).

  The other functions return an object with the relevant statistic or
  effect size, with a confidence interval for the effect size.
  
  For computeStatistic, this object always contains:
  \item{statistic}{The relevant statistic}
  \item{statistic.type}{The type of statistic}
  \item{parameter}{The degrees of freedom for this statistic}
  \item{p.raw}{The p-value of this statistic for NHST}
  And in addition, it often contains (among other things, sometimes):
  \item{object}{The object from which the statistics are extracted}
  
  For computeEffectSize, this object always contains:
  \item{es}{The point estimate for the effect size}
  \item{esc.type}{The type of effect size}
  \item{ci}{The confidence interval for the effect size}
  And in addition, it often contains (among other things, sometimes):
  \item{object}{The object from which the effect size is extracted}
  
}
\examples{

computeStatistic_f(Orange$Tree, Orange$circumference)
computeEffectSize_etasq(Orange$Tree, Orange$circumference)

}
\keyword{ utilities }
\keyword{ bivar }