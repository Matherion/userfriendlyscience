\name{rMatrix}
\alias{rMatrix}
\title{
rMatrix
}
\description{
rMatrix provides a correlation matrix with confidence intervals and a p-value adjusted for multiple testing.
}
\usage{
rMatrix(dat, x, y=NULL, conf.level = .95, correction = "fdr",
        digits = 2, pValueDigits=3, colspace=2, rowspace=0,
        colNames ="numbers",
        output="R",
        env.LaTeX = 'tabular',
        pboxWidthMultiplier = 1)
}
\arguments{
  \item{dat}{
  A dataframe containing the relevant variables.
}
  \item{x}{
  Vector of 1+ variable names.
}
  \item{y}{
  Vector of 1+ variable names; if this is left empty, a symmetric matrix is created; if this is filled, the matrix will have the x variables defining the rows and the y variables defining the columns.
}
  \item{conf.level}{
  The confidence of the confidence intervals.
}
  \item{correction}{
  Correction for multiple testing: an element out of the vector c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"). NOTE: the p-values are corrected for multiple testing; The confidence intervals are not (yet :-)).
}
  \item{digits}{
  With what precision do you want the results to print.
}
  \item{pValueDigits}{
  Determines the number of digits to use when displaying p values. P-values that are too small will be shown as p<.001 or p<.00001 etc.
}
  \item{colspace}{
  Number of spaces between columns (only for R output, ignored for LaTeX output)
}
  \item{rowspace}{
  Number of rows between table rows (note: one table row is 2 rows; only for R output, ignored for LaTeX output).
}
  \item{colNames}{
  colNames can be "numbers" or "names". "Names" cause variables names
  to be printed in the heading; "numbers" causes the rows to become
  numbered and the numbers to be printed in the heading.
}
  \item{output}{
  Can be "R" or "LaTeX"; if output is set to "LaTeX", the result is a LaTeX table (e.g. for use
  in knitr).
}
  \item{env.LaTeX}{
  For LaTeX output, the environment can be set with env.LaTeX.
}
  \item{pboxWidthMultiplier}{
  When using LaTeX, pboxWidthMultiplier can be used to make the cells
  narrower or wider (1 works for anything up until 4 or 5 digits).
}
}
\details{

rMatrix provides a symmetric or asymmetric matrix of correlations their confidence intervals, and p-values. The p-values can be corrected for multiple testing.

}
\value{

An object with the input and several output variables. Most notably a number of matrices:
  \item{r}{Pearson r values.}
  \item{parameter}{Degrees of freedom.}
  \item{ci.lo}{Lower bound of Pearson r confidence interval.}
  \item{ci.hi}{Upper bound of Pearson r confidence interval.}
  \item{p.raw}{Original p-values.}
  \item{p.adj}{p-values adjusted for multiple testing.}
}
\examples{

rMatrix(mtcars, x=c('disp', 'hp', 'drat'))

}
\keyword{ utilities }
