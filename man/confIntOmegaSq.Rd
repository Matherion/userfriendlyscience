\name{confIntOmegaSq}
\alias{confIntOmegaSq}
\title{
Confidence intervals for Omega Squared
}
\description{
This function used the \link{MBESS} function \code{\link{conf.limits.ncf}} and \code{\link{convert.ncf.to.omegasq}} to compute the point estimate and confidence interval for Omega Squared.
}
\usage{
confIntOmegaSq(var1, var2, conf.level = 0.95)
}
\arguments{
  \item{var1, var2}{
  The two variables: one should be a factor (or will be made a factor), the other should have at least interval level of measurement. If none of the variables is a factor, the function will look for the variable with the least unique values and change it into a factor.
}
  \item{conf.level}{
  Level of confidence for the confidence interval.
}
}
\value{

A \code{confIntOmegaSq} object is returned, with as elements:

\item{input}{The input arguments}
\item{intermediate}{Objects generated while computing the output}
\item{output}{The output of the function, consisting of:}
\item{output$es}{The point estimate}
\item{output$ci}{The confidence interval}
}
\note{
Formula 16 in Steiger (2004) is used for the conversion in \code{\link{convert.ncf.to.omegasq}}.
}
\references{
Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals and tests of close fit in the analysis of variance and contrast analysis. Psychological Methods, 9(2), 164-82. https://doi.org/10.1037/1082-989X.9.2.164
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{

confIntOmegaSq(mtcars$mpg, mtcars$cyl);

}
\keyword{ bivar }
