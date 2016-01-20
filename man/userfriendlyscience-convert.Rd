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
\title{
convert functions
}
\description{
These are a number of functions to convert statistics and effect size measures from/to each other.
}
\usage{
convert.r.to.t(r, n)
convert.r.to.d(r)
convert.r.to.p(r, n)
convert.r.to.fisherz(r)

convert.t.to.r(t, n)
convert.t.to.d(t, df=NULL, n1=NULL, n2=NULL, proportion=.5)
convert.t.to.p(t, df)

convert.f.to.p(f, df1, df2, lower.tail=FALSE)
convert.f.to.d(f, df1, df2 = NULL, n1=NULL, n2=NULL, proportion=.5)

convert.chisq.to.p(chisq, df, lower.tail=FALSE)
convert.chisq.to.V(chisq, n, minDim)

convert.d.to.logodds(d)
convert.d.to.r(d, n1 = NULL, n2 = NULL)

convert.etasq.to.cohensf(etasq)

convert.f.to.etasq(f, df1, df2)
convert.f.to.omegasq(f, df1, df2)

convert.or.to.d(or)
convert.or.to.r(or)

convert.fisherz.to.r(z)

convert.b.to.t(b, se)

convert.logodds.to.d(logodds)
convert.logodds.to.r(logodds)

}
\arguments{
  \item{r, t, f, chisq, d, etasq, z, or, logodds}{
  The value of the relevant statistic or effect size.
}
  \item{n, n1, n2}{
  The number of observations that the r or t value is based on, of the
  number of observations in each of the two groups for an anova.
}
  \item{df, df1, df2}{
  The degrees of freedrom for that statistic (for F, the first one is the numerator (i.e. the effect), and the second one the denominator (i.e. the error term).
}
  \item{proportion}{
  The proportion of participants in each of the two groups in an anova. This
  is used to compute the sample size in each group if the group sizes for an
  anova are unknown. Thus, if you only provide df1 and df2 when converting
  an F value to a Cohen's d value, equal group sizes are assumed.
  }
  \item{b}{
  The value of a regression coefficient.
}
  \item{se}{
  The standard error of the relevant statistic (e.g. of a regression coefficient).
}
  \item{minDim}{
  The smallest of the number of columns and the number of rows of the crosstable
  for which the chisquare is translated to a Cramer's V value.
}
  \item{lower.tail}{
  For the F and chisquare distributions, whether to get the probability of the lower or upper tail.
}
}
\value{

The converted value as a numeric value.

}
\examples{

convert.t.to.r(t=-6.46, n=200);
convert.r.to.t(r=-.41, n=200);

### Compute some p-values
convert.t.to.p(4.2, 197);
convert.chisq.to.p(5.2, 3);
convert.f.to.p(8.93, 3, 644);

}
\keyword{ utilities }
