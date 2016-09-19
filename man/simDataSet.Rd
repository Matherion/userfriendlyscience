\name{simDataSet}
\alias{simDataSet}
\title{
simDataSet
}
\description{
simDataSet can be used to conveniently and quickly simulate a dataset that
satisfies certain constraints, such as a specific correlation structure,
means, ranges of the items, and measurement levels of the variables. Note
that the results are approximate; mvrnorm is used to generate the correlation
matrix, but the factor are only created after that, so cutting the variable
into factors may change the correlations a bit.
}
\usage{
simDataSet(n,
           varNames,
           correlations = c(0.1, 0.4),
           specifiedCorrelations = NULL,
           means = 0,
           sds = 1,
           ranges = c(1, 7),
           factors = NULL,
           cuts = NULL,
           labels = NULL,
           seed = 20160503,
           empirical = TRUE,
           silent = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of requires cases (records, entries, participants, rows) in the final dataset.
}
  \item{varNames}{
Names of the variables in a vector; note that the length of this vector will determine the number of variables simulated.
}
  \item{correlations}{
The correlations between the variables are randomly sampled from this range using the uniform distribution; this way, it's easy to have a relatively 'messy' correlation matrix without the need to specify every correlation manually.
}
  \item{specifiedCorrelations}{
The correlations that have to have a specific value can be specified here, as a list of vectors, where each vector's first two elements specify variables names, and the last one the correlation between those two variables. Note that tweaking the correlations may take some time; the \code{\link{mvrnorm}} function will complain that "'Sigma' is not positive definite", or in other words, you supplied a combination of correlations that can't exist simultaneously, if you get it wrong.
}
  \item{means, sds}{
The means and standard deviations of the variables. Note that is you set \code{ranges} for one or more variables (see below), those ranges are used to rescale those variables, overriding any specified means and standard deviations. If only one mean or standard deviation is supplied, it's recycled along the variables.
}
  \item{ranges}{
The desired ranges of the variables, supplied as a named list where the name of each element corresponds to a variable. The \code{\link{rescale}} function will be used to rescale those variables for which a desired scale is specified here. Note that for those variables, the means and standard deviations will be determined by these new ranges.
}
  \item{factors}{
A vector of variable names that should be converted into factors (using \code{\link{cut}}). Make sure to specify lists for \code{cuts} and \code{labels} as well (of the same length).
}
  \item{cuts}{
A list of vectors that specify, for each factor, where to 'cut' the numeric vector into factor levels.
}
  \item{labels}{
A list of vectors that specify, for each factor, and for each level, the labels that should be assigned to the factor levels. Each vector in this list has to have one more element than each vector in the \code{cuts} list.
}
  \item{seed}{
The seed to use when generating the dataset (to make sure the exact same dataset can be generated repeatedly).
}
  \item{empirical}{
Whether to generate the data using the exact (\code{empirical = TRUE}) or approximate (\code{empirical = FALSE}) correlation matrix; this is passed on to \code{\link{mvrnorm}}.
}
  \item{silent}{
Whether to show intermediate and final descriptive information (correlation and covariance matrices as well as summaries).
}
}
\details{
This function was intended to allow relatively quick generation of datasets that satisfy specific constraints, e.g. including a number of factors, variables with a specified minimum and maximum value or specified means and standard deviations, and of course specific correlations. Because all correlations except those specified are randomly generated from a uniform distribution, it's quite convenient to generate messy kind of real looking datasets quickly. Note that it's mostly a convenience function, and datasets will still require tweaking; for example, factors are simply numeric vectors that are \code{\link{cut}} *after* \code{\link{mvrnorm}} generated the data, so the associations will change slightly.
}
\value{
The generated dataframe is returned invisibly.
}
\author{
Gjalt-Jorn Peters
}
\seealso{
\code{\link{mvrnorm}}
}
\examples{
dat <- simDataSet(500, varNames=c('age',
                                  'sex',
                                  'educationLevel',
                                  'negativeLifeEventsInPast10Years',
                                  'problemCoping',
                                  'emotionCoping',
                                  'resilience',
                                  'depression'),
                  means = c(40,
                            0,
                            0,
                            5,
                            3.5,
                            3.5,
                            3.5,
                            3.5),
                  sds = c(10,
                          1,
                          1,
                          1.5,
                          1.5,
                          1.5,
                          1.5,
                          1.5),
                  specifiedCorrelations =
                    list(c('problemCoping', 'emotionCoping', -.5),
                         c('problemCoping', 'resilience', .5),
                         c('problemCoping', 'depression', -.4),
                         c('depression', 'emotionCoping', .6),
                         c('depression', 'resilience', -.3)),
                  ranges = list(age = c(18, 54),
                                negativeLifeEventsInPast10Years = c(0,8),
                                problemCoping = c(1, 7),
                                emotionCoping = c(1, 7)),
                  factors=c("sex", "educationLevel"),
                  cuts=list(c(0),
                            c(-.5, .5)),
                  labels=list(c('female', 'male'),
                              c('lower', 'middle', 'higher')),
                  silent=FALSE);
}
\keyword{ utilities }
