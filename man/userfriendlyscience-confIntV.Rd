\name{confIntV}
\alias{confIntV}
\alias{cramersV}
\alias{crossTab}
\title{
crossTab, confIntV and cramersV
}
\description{
These functions compute the point estimate and confidence interval for
Cramer's V. The crossTab function also shows a crosstable.
}
\usage{
crossTab(x, y=NULL, conf.level=.95,
         digits=2, pValueDigits=3, ...)
cramersV(x, y = NULL, digits=4)
confIntV(x, y = NULL, conf.level=.95,
         samples = 500, digits=4,
         method=c('bootstrap', 'fisher'),
         storeBootstrappingData = FALSE)
}
\arguments{
  \item{x}{
  Either a crosstable to analyse, or one of two vectors to use to generate
  that crosstable. The vector should be a factor, i.e. a categorical
  variable identified as such by the 'factor' class).
}
  \item{y}{
  If x is a crosstable, y can (and should) be empty. If x is a vector, y
  must also be a vector.
}
  \item{digits}{
  Minimum number of digits after the decimal point to show in the result.
}
  \item{pValueDigits}{
  Minimum number of digits after the decimal point to show in the Chi
  Square p value in the result.
}
  \item{conf.level}{
  Level of confidence for the confidence interval.
}
  \item{samples}{
  Number of samples to generate when bootstrapping.
}
  \item{method}{
  Whether to use Fisher's Z or bootstrapping to compute the confidence
  interval.
}
  \item{storeBootstrappingData}{
  Whether to store (or discard) the data generating during the bootstrapping
  procedure.
}
  \item{...}{
  Extra arguments to \code{crossTab} are passed on to \code{confIntV}.
}
}
\value{

The cramersV and confIntV functions return either a point estimate or
a confidence interval for Cramer's V, an effect size to describe the 
association between two categorical variables. The crossTab function is
just a wrapper around confIntV.

}
\examples{

### Get confidence interval for Cramer's V
### Note that removing the 'table', and so providing raw data, would enable
### bootstrapping, which can take a while.
confIntV(table(infert$education, infert$induced));

}
\keyword{ bivar }
