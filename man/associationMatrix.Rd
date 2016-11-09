\name{associationMatrix}
\alias{associationMatrix}
\title{
associationMatrix
}
\description{
associationMatrix produces a matrix with confidence intervals for effect
sizes, point estimates for those effect sizes, and the p-values for the
test of the hypothesis that the effect size is zero, corrected for
multiple testing.
}
\usage{
associationMatrix(dat=NULL, x=NULL, y=NULL, conf.level = .95,
                  correction = "fdr", bootstrapV=FALSE,
                  info=c("full", "ci", "es"),
                  includeSampleSize = "depends",
                  bootstrapV.samples = 5000, digits = 2,
                  pValueDigits = digits + 1, colNames = FALSE,
                  type=c("R", "html", "latex"), file="",
                  statistic = associationMatrixStatDefaults,
                  effectSize = associationMatrixESDefaults,
                  var.equal = 'test')
}
\arguments{
  \item{dat}{
  A dataframe with the variables of interest. All variables in this
  dataframe will be used if both x and y are NULL. If dat is NULL, the user
  will be presented with a dialog to select a datafile.
}
  \item{x}{
  If not NULL, this should be a character vector with the names of the
  variables to include in the rows of the association table. If x is
  NULL, all variables in the dataframe will be used.
}
  \item{y}{
  If not NULL, this should be a character vector with the names of the
  variables to include in the columns of the association table. If y is
  NULL, the variables in x will be used for the columns as well (which
  produces a symmetric matrix, similar to most correlation matrices).

}
  \item{conf.level}{
  Level of confidence of the confidence intervals.
}
  \item{correction}{
  Correction for multiple testing: an element out of the vector
  c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none").
  NOTE: the p-values are corrected for multiple testing;
  The confidence intervals are not!
}
  \item{bootstrapV}{
  Whether to use bootstrapping to compue the confidence interval for
  Cramer's V or whether to use the Fisher's Z conversion.
}
  \item{info}{
    Information to print: either both the confidence interval and the
    point estimate for the effect size (and the p-value, corrected for
    multiple testing), or only the confidence intervals, or only the point
    estimate (and the corrected p-value). Must be on element of the vector
    c("full", "ci", "es").
}
  \item{includeSampleSize}{
  Whether to include the sample size when the effect size point estimate and
  p-value are shown. If this is "depends", it will depend on whether all
  associations have the same sample size (and the sample size will only be
  printed when they don't). If "always", the sample size will always be added.
  If anything else, it will never be printed.
}
  \item{bootstrapV.samples}{
  If using boostrapping for Cramer's V, the number of samples to generate.
}
  \item{digits}{
  Number of digits to round to when printing the results.
}
  \item{pValueDigits}{
  How many digits to use for formatting the p values.
  }
  \item{colNames}{
  If true, the column heading will use the variables names instead of
  numbers.
}
  \item{type}{
  Type of output to generate: must be an element of the vector
  c("R", "html", "latex").
}
  \item{file}{
  If a file is specified, the output will be written to that file instead
  of shown on the screen.
}
  \item{statistic}{
  This is the complicated bit; this is where associationMatrix allows
  customization of the used statistics to perform null hypothesis
  significance testing. For everyday use, leaving this at the default value,
  associationMatrixStatDefaults, works fine. In case you want to customize,
  read the 'Notes' section below.
  
}
  \item{effectSize}{
  Like the 'statistics' argument, 'effectSize also allows customization,
  in this case of the used effect sizes. Again, the default value,
  associationMatrixESDefaults, works for everyday use. Again, see the
  'Notes' section below if you want to customize.
}
  \item{var.equal}{
  Whether to test for equal variances ('test'), assume equality ('yes'), or assume unequality ('no'). See \code{\link{meanDiff}} for more information.
  }
}
\value{

  An object with the input and several output variables, one of which is
  a dataframe with the association matrix in it. When this object is
  printed, the association matrix is printed to the screen. If the 'file'
  parameter is specified, a file with this matrix will also be written to
  disk.
  
}
\note{

  The 'statistic' and 'effectSize' parameter make it possible to use
  different functions to conduct null hypothesis significance testing and
  compute effect sizes. In both cases, the parameter needs to be a list
  containing four lists, named
  'dichotomous', 'nominal', 'ordinal', and 'interval'. Each of these
  lists has to contain four elements, character vectors of length one (i.e.
  just one string value), again named 'dichotomous', 'nominal', 'ordinal',
  and 'interval'.
  
  The combination of each of these names (e.g.
  'dichotomous' and 'nominal', or 'ordinal' and 'interval', etc) determine
  which test should be done when computing the p-value to test the
  association between two variables of those types, or which effect sizes
  to compute. When called, associationMatrix determines the measurement
  levels of the relevant variables. It then uses these two levels
  (their string representation, e.g. 'dichotomous' etc) to find a string
  in the 'statistic' and 'effectSize' objects. Two functions with these
  names are then called from two lists, 'computeStatistic' and
  computeEffectSize. These lists list contain functions that have the
  same names as the strings in the 'statistic' list.
  
  For example, when
  the default settings are used, the string (function name) found for two
  dichotomous variables when searching in associationMatrixStatDefaults
  is 'chisq', and the string found in associationMatrixESDefaults is 'v'.
  associationMatrix then calls computeStatistic[['chisq']] and
  computeEffectSize[['v']], providing the two variables as arguments, as
  well as passing the 'conf.level' argument. These two functions then
  each return an object that associationMatrix extracts the information
  from. Inspect the source code of these functions (by typing their names
  without parentheses in the R prompt) to learn how this object should
  look, if you want to write your own functions.

}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{

### Generate a simple association matrix using all three variables in the
### Orange tree dataframe
associationMatrix(Orange);

### Or four variables from infert:
associationMatrix(infert, c("education", "parity",
                            "induced", "case"), colNames=TRUE);

### Use variable names in the columns and generate html
associationMatrix(Orange, colNames=TRUE, type='html');

}
\keyword{ utilities }
\keyword{ univar }
