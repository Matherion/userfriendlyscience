\name{sharedSubString}
\alias{sharedSubString}
\title{
sharedSubString
}
\description{
A function to find the longest shared substring in a character vector.
}
\usage{
sharedSubString(x, y = NULL)
}
\arguments{
  \item{x}{
  The character vector to process.
}
  \item{y}{
  Optionally, two single values can be specified. This is probably not useful to end users, but it's used by the function when it calls itself.
}
}
\value{
A vector of length one with either the longest substring that occurs in all values of the character vector, or NA if no overlap an be found.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
  sharedSubString(c("t0_responseTime", "t1_responseTime", "t2_responseTime"));
  ### Returns "_responseTime"
}
\keyword{ character }
