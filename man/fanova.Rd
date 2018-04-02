\name{fanova}
\alias{fanova}
\title{
Flexible anova
}
\description{
This function is meant as a userfriendly wrapper to approximate the way analysis of variance is done in SPSS.
}
\usage{
fanova(data,
       y,
       between = NULL,
       covar = NULL,
       plot = FALSE,
       levene = FALSE,
       digits = 2,
       contrast = NULL)
}
\arguments{
  \item{data}{
  The dataset containing the variables to analyse.
}
  \item{y}{
  The dependent variable. For oneway anova, factorial anova, or ancova, this is the name of a variable in dataframe \code{data}. For repeated measures anova, this is a vector with the names of all variable names in dataframe \code{data}, e.g. \code{c('t0_value', 't1_value', 't2_value')}.
}
  \item{between}{
  A vector with the variables name(s) of the between subjects factor(s).
}
  \item{covar}{
  A vector with the variables name(s) of the covariate(s).
}
  \item{plot}{
  Whether to produce a plot. Note that a plot is only produced for oneway and twoway anova and oneway repeated measures designs: if covariates or more than two between-subjects factors are specified, not plot is produced. For twoway anova designs, the second predictor is plotted as moderator (and the first predictor is plotted on the x axis).
}
  \item{levene}{
Whether to show Levene's test for equality of variances (using \code{\link[car]{car}}'s \code{\link{leveneTest}} function but specifying \code{\link{mean}} as function to compute the center of each group).
}
  \item{digits}{
  Number of digits (actually: decimals) to use when printing results. The p-value is printed with one extra digit.
}
  \item{contrast}{
  This functionality has not been implemented yet.
}
}
\details{
This wrapper uses \code{\link{oneway}} and \code{\link{lm}} and \code{\link{lmer}} in combination with \code{\link[car]{car}}'s \code{\link{Anova}} function to conduct the analysis of variance.
}
\value{
Mainly, this function prints its results, but it also returns them in an object containing three lists:
  \item{input}{The arguments specified when calling the function}
  \item{intermediate}{Intermediat objects and values}
  \item{output}{The results such as the plot.}
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{regr}} and \code{\link{logRegr}} for similar functions for linear and logistic regression and \code{\link{oneway}}, \code{\link{lm}}, \code{\link{lmer}} and \code{\link{Anova}} for the functions used behind the scenes.
}
\examples{
### Oneway anova with a plot
fanova(dat=mtcars, y='mpg', between='cyl', plot=TRUE);

### Factorial anova
fanova(dat=mtcars, y='mpg', between=c('vs', 'am'), plot=TRUE);

### Ancova
fanova(dat=mtcars, y='mpg', between=c('vs', 'am'), covar='hp');

### Repeated measures anova; first generate datafile
dat <- mtcars[, c('am', 'drat', 'wt')];
names(dat) <- c('factor', 't0_dependentVar' ,'t1_dependentVar');
dat$factor <- factor(dat$factor);

### Then do the repeated measures anova
fanova(dat, y=c('t0_dependentVar' ,'t1_dependentVar'),
       between='factor', plot=TRUE);

}
\keyword{ htest }
\keyword{ hplot }
