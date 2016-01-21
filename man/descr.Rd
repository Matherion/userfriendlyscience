\name{descr}
\alias{descr}
\alias{descriptives}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
descr (or descriptives)
}
\description{
This function provides a number of descriptives about your data, similar to
what SPSS's DESCRIPTIVES (often called with DESCR) does.
}
\usage{
descr(x, digits = 4, errorOnFactor = FALSE,
      include = c("central tendency", "spread", "range",
                  "distribution shape", "sample size"));
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  The vector for which to return descriptives.
}
  \item{digits}{
  The number of digits to round the results to when showing them.
}
  \item{errorOnFactor}{
  Whether to show an error when the vector is a factor, or just show the
  frequencies instead.
}
  \item{include}{
  Which elements to include when showing the results.
}
}
\details{
Note that R (of course) has many similar functions, such as \code{\link{summary}},
\code{\link{describe}} in the excellent \code{\link{psych}} package.
}
\value{
A list of dataframes with the requested values. 
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{summary}}, \code{\link{describe}}
}
\examples{
descr(mtcars$mpg);
}
\keyword{ univariate }
