\name{convert}
\alias{convert}
\alias{convert.r.to.t}
\alias{convert.t.to.r}
\alias{convert.b.to.t}
\alias{convert.t.to.p}
\alias{convert.f.to.p}
\alias{convert.f.to.d}
\alias{convert.chisq.to.p}
\alias{convert.chisq.to.V}
\alias{convert.d.to.logodds}
\alias{convert.d.to.r}
\alias{convert.d.to.t}
\alias{convert.d.to.variance}
\alias{convert.etasq.to.cohensf}
\alias{convert.f.to.etasq}
\alias{convert.f.to.omegasq}
\alias{convert.fisherz.to.r}
\alias{convert.logodds.to.d}
\alias{convert.logodds.to.r}
\alias{convert.or.to.d}
\alias{convert.or.to.r}
\alias{convert.r.to.d}
\alias{convert.r.to.fisherz}
\alias{convert.r.to.p}
\alias{convert.t.to.d}
\alias{convert.cohensf.to.omegasq}
\alias{convert.cohensfsq.to.omegasq}
\alias{convert.means.to.d}
\alias{convert.ncf.to.omegasq}
\alias{convert.omegasq.to.cohensf}
\alias{convert.omegasq.to.cohensfsq}
\alias{convert.omegasq.to.f}
\alias{convert.percentage.to.se}
\title{
conversion functions
}
\description{
These are a number of functions to convert statistics and effect size measures from/to each other.
}
\usage{
convert.b.to.t(b, se)

convert.chisq.to.p(chisq, df, lower.tail=FALSE)
convert.chisq.to.V(chisq, n, minDim)

convert.cohensf.to.omegasq(cohensf)

convert.cohensfsq.to.omegasq(cohensfsq)

convert.d.to.logodds(d)
convert.d.to.r(d, n1 = NULL, n2 = NULL, akfEq8='if (n1 + n2) < 50')
convert.d.to.t(d, df = NULL, n1 = NULL, n2 = NULL, proportion = 0.5)
convert.d.to.variance(d, n1, n2)

convert.etasq.to.cohensf(etasq)

convert.f.to.etasq(f, df1, df2)
convert.f.to.omegasq(f, df1, df2)
convert.f.to.p(f, df1, df2, lower.tail=FALSE)
convert.f.to.d(f, df1, df2 = NULL, n1=NULL, n2=NULL, proportion=.5)

convert.fisherz.to.r(z)

convert.logodds.to.d(logodds)
convert.logodds.to.r(logodds)

convert.means.to.d(means, sds, ns = NULL, var.equal = NULL)

convert.ncf.to.omegasq(ncf, N)

convert.omegasq.to.cohensf(omegasq)
convert.omegasq.to.cohensfsq(omegasq)
convert.omegasq.to.f(omegasq, df1, df2)

convert.or.to.d(or)
convert.or.to.r(or)

convert.percentage.to.se(p, n)

convert.r.to.t(r, n)
convert.r.to.d(r)
convert.r.to.p(r, n)
convert.r.to.fisherz(r)

convert.t.to.r(t, n)
convert.t.to.d(t, df=NULL, n1=NULL, n2=NULL, proportion=.5)
convert.t.to.p(t, df)

}
\arguments{
  \item{chisq, cohensf, cohensfsq, d, etasq, f, logodds, means, omegasq, or, p, r, t, z}{
  The value of the relevant statistic or effect size.
}
  \item{ncf}{
  The value of a noncentrality parameter of the F distribution.
  }
  \item{n, n1, n2, N, ns}{
  The number of observations that the r or t value is based on, or the
  number of observations in each of the two groups for an anova, or the total number of participants when specifying a noncentrality parameter.
}
  \item{df, df1, df2}{
  The degrees of freedrom for that statistic (for F, the first one is the numerator (i.e. the effect), and the second one the denominator (i.e. the error term).
}
  \item{proportion}{
  The proportion of participants in each of the two groups in a t-test or anova. This
  is used to compute the sample size in each group if the group sizes are unknown.
  Thus, if you only provide df1 and df2 when converting
  an F value to a Cohen's d value, equal group sizes are assumed.
  }
  \item{b}{
  The value of a regression coefficient.
}
  \item{se, sds}{
  The standard error of standard errors of the relevant statistic (e.g. of a regression coefficient) or variables.
}
  \item{minDim}{
  The smallest of the number of columns and the number of rows of the crosstable
  for which the chisquare is translated to a Cramer's V value.
}
  \item{lower.tail}{
  For the F and chisquare distributions, whether to get the probability of the lower or upper tail.
}
  \item{akfEq8}{
  When converting Cohen's \emph{d} to \emph{r}, for small sample sizes, bias is introduced when the commonly suggested formula is used (Aaron, Kromrey & Ferron, 1998). Therefore, by default, this function uses different equations depending on the sample size (for n < 50 and for n > 50). When \code{akfEq8} is set to TRUE or FALSE, the corresponding action is taken; when \code{akfEq8} is not logical (i.e. TRUE or FALSE), the function depends on the sample size.
}
  \item{var.equal}{
  Whether to compute the value of \emph{t} or Cohen's \emph{d} assuming equal variances ('yes'), unequal variances ('no'), or whether to test for the difference ('test').
}
}
\details{
Note that by default, the behavior of \code{convert.d.to.r} depends on the sample size (see Bruce, Kromrey & Ferron, 1998).
}
\value{

The converted value as a numeric value.

}
\references{
Aaron, B. Kromrey J. D. & Ferron, J. (1998) \emph{Equating "r"-based and "d"-based Effect Size Indices: Problems with a Commonly Recommended Formula.} Paper presented at the Annual Meeting of the Florida Educational Research Association (43rd, Orlando, FL, November 2-4, 1998).
}
\author{
Gjalt-Jorn Peters and Peter Verboon

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
convert.t.to.r(t=-6.46, n=200);
convert.r.to.t(r=-.41, n=200);

### Compute some p-values
convert.t.to.p(4.2, 197);
convert.chisq.to.p(5.2, 3);
convert.f.to.p(8.93, 3, 644);

### Convert d to r using both equations
convert.d.to.r(d=.2, n1=5, n2=5, akfEq8 = FALSE);
convert.d.to.r(d=.2, n1=5, n2=5, akfEq8 = TRUE);
}
\keyword{ utilities }
