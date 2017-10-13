\name{logRegr}
\alias{logRegr}
\title{
Userfriendly wrapper to do logistic regression in R
}
\description{
This function is meant as a userfriendly wrapper to approximate the way logistic regression is done in SPSS.
}
\usage{
logRegr(formula,
        data = NULL,
        conf.level = 0.95,
        digits = 2,
        pvalueDigits = 3,
        crossTabs = TRUE,
        plot = FALSE,
        collinearity = FALSE,
        env = parent.frame(),
        predictionColor = viridis(3)[3],
        predictionAlpha = 0.5,
        predictionSize = 2,
        dataColor = viridis(3)[1],
        dataAlpha = 0.33,
        dataSize = 2,
        observedMeansColor = viridis(3)[2],
        binObservedMeans = 7,
        observedMeansSize = 2,
        observedMeansWidth = NULL,
        observedMeansAlpha = 0.5,
        theme = theme_bw())
}
\arguments{
  \item{formula}{
  The formula, specified in the same way as for \code{\link{glm}} (which is used for the actual analysis).
}
  \item{data}{
  Optionally, a dataset containing the variables in the formula (if not specified, the variables must exist in the environment specified in \code{env}.
}
  \item{conf.level}{
  The confidence level for the confidence intervals.
}
  \item{digits}{
  The number of digits used when printing the results.
}
  \item{pvalueDigits}{
  The number of digits used when printing the p-values.
}
  \item{crossTabs}{
  Whether to show cross tabulations of the correct predictions for the null model and the tested model, as well as the percentage of correct predictions.
}
  \item{plot}{
  Whether to display the plot.
}
  \item{collinearity}{
  Whether to show collinearity diagnostics.
}
  \item{env}{
  If no dataframe is specified in \code{data}, use this argument to specify the environment holding the variables in the formula.
}
  \item{predictionColor, dataColor, observedMeansColor}{
  The color of, respectively, the line and confidence interval showing the prediction; the points representing the observed data points; and the means based on the observed data.
}
  \item{predictionAlpha, dataAlpha, observedMeansAlpha}{
  The alpha of, respectively, the confidence interval of the prediction; the points representing the observed data points; and the means based on the observed data (set to 0 to hide an element).
}
  \item{predictionSize, dataSize, observedMeansSize}{
  The size of, respectively, the line of the prediction; the points representing the observed data points; and the means based on the observed data (set to 0 to hide an element).
}
  \item{binObservedMeans}{
  Whether to bin the observed means; either FALSE or a single numeric value specifying the number of bins.
}
  \item{observedMeansWidth}{
  The width of the lines of the observed means. If not specified (i.e. \code{NULL}), this is computed automatically and set to the length of the shortest interval between two successive points in the predictor data series (found using \code{\link{findShortestInterval}}.
}
  \item{theme}{
  The theme used to display the plot.
}
}
\details{
  This function
}
\value{
Mainly, this function prints its results, but it also returns them in an object containing three lists:
  \item{input}{The arguments specified when calling the function}
  \item{intermediate}{Intermediat objects and values}
  \item{output}{The results, such as the plot, the cross tables, and the coefficients.}
}
\author{
Ron Pat-El & Gjalt-Jorn Peters (both while at the Open University of the Netherlands)

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{regr}} and \code{\link{fanova}} for similar functions for linear regression and analysis of variance and \code{\link{glm}} for the regular interface for logistic regression.
}
\examples{
### Simplest way to call logRegr
logRegr(data=mtcars, formula = vs ~ mpg);

### Also ordering a plot
logRegr(data=mtcars, formula = vs ~ mpg, plot=TRUE);

### Only use five bins
logRegr(data=mtcars, formula = vs ~ mpg, plot=TRUE, binObservedMeans=5);
}
\keyword{ htest }
\keyword{ models }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ hplot }
