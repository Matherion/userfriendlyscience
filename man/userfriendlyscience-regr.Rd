\name{regr}
\alias{regr}
\title{
regr: a simple regression analysis wrapper
}
\description{
The \code{regr} function wraps a number of linear regression functions into
one convenient interface that provides similar output to the regression
function in SPSS. It automatically provides confidence intervals and
standardized coefficients. Note that this function is meant for teaching
purposes, and therefore it's only for very basic regression analyses.
}
\usage{
regr(formula, dat=NULL, conf.level=.95, digits=2,
     pvalueDigits = 3, coefficients=c("raw", "scaled"))
}
\arguments{
  \item{formula}{
The formula of the regression analysis, of the form \code{y ~ x1 + x2}, where
y is the dependent variable and x1 and x2 are the predictors.
}
  \item{dat}{
If the terms in the formula aren't vectors but variable names, this should
be the dataframe where those variables are stored.
}
  \item{conf.level}{
The confidence of the confidence interval around the regression coefficients.
}
  \item{digits}{
Number of digits to round the output to.
}
  \item{pvalueDigits}{
The number of digits to show for p-values; smaller p-values will be shown as
<.001 or <.0001 etc.
}
  \item{coefficients}{
Which coefficients to show; can be "raw" to only show the raw
(unstandardized) coefficients; "scaled" to only show the scaled
(standardized) coefficients), or c("raw", "scaled') to show both.
}
}
\value{
A list of three elements:
\item{input}{List with input arguments}
\item{intermediate}{List of intermediate objects, such as the lm and
confint objects.}
\item{output}{List with two dataframes, one with the raw coefficients,
and one with the scaled coefficients.}
}
\examples{
### Do a simple regression analysis
regr(age ~ circumference, dat=Orange);

### Show more digits for the p-value
regr(Orange$age ~ Orange$circumference, pvalueDigits=18);
}
\keyword{ utilities }