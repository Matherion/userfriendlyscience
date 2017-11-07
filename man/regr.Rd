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
regr(formula, data = NULL, conf.level = .95,
     digits = 2, pvalueDigits = 3,
     coefficients = c("raw", "scaled"),
     plot = FALSE, pointAlpha = .5,
     collinearity = FALSE, influential = FALSE,
     ci.method = c("widest", "r.con", "olkinfinn"),
     ci.method.note = FALSE, env = parent.frame())
}
\arguments{
  \item{formula}{
The formula of the regression analysis, of the form \code{y ~ x1 + x2}, where
y is the dependent variable and x1 and x2 are the predictors.
}
  \item{data}{
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
  \item{plot}{
  For regression analyses with only one predictor (also sometimes confusingly
  referred to as 'univariate' regression analyses), scatterplots with
  regression lines and their standard errors can be produced.
}
  \item{pointAlpha}{
  The alpha channel (transparency, or rather: 'opaqueness') of the points
  drawn in the plot.
}
  \item{collinearity}{
  Whether to compute and show collinearity diagnostics (specifically, the tolerance (\emph{1 - R^2}, where \emph{R^2} is the one obtained when regressing each predictor on all the other predictors) and the Variance Inflation Factor (VIF), which is the reciprocal of the tolerance, i.e. \emph{VIF = 1 / tolerance}).
}
  \item{influential}{
  Whether to compute diagnostics for influential cases. These are stored in the returned object in the \code{lm.influence.raw} and \code{lm.influence.scaled} objects in the \code{intermediate} object.
}
  \item{ci.method, ci.method.note}{
  Which method to use for the confidence interval around R squared, and whether to display a note about this choice.
}
  \item{env}{
  The enviroment where to evaluate the formula.
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
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}

\examples{
### Do a simple regression analysis
regr(age ~ circumference, dat=Orange);

### Show more digits for the p-value
regr(Orange$age ~ Orange$circumference, pvalueDigits=18);
}
\keyword{ utilities }
