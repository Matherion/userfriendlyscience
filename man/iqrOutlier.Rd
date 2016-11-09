\name{iqrOutlier}
\alias{iqrOutlier}
\title{
Identify outliers according to the IQR criterion
}
\description{
The IQR criterion holds that any value lower than one-and-a-half times the interquartile range below the first quartile, or higher than one-and-a-half times the interquartile range above the third quartile, is an outlier. This function returns a logical vector that identifies those outliers.
}
\usage{
iqrOutlier(x)
}
\arguments{
  \item{x}{
  The vector to scan for outliers.
}
}
\value{
A logical vector where TRUE identifies outliers.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{IQR}}
}
\examples{
### One outlier in the miles per gallon
iqrOutlier(mtcars$mpg);
}
\keyword{ univariate }
