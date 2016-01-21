\name{exceptionalScore}
\alias{exceptionalScore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
exceptionalScore
}
\description{
This function can be used to detect exceptionally high or low scores in a vector.
}
\usage{
exceptionalScore(x, prob = 0.025, both = TRUE, silent = FALSE,
                 quantileCorrection = 1e-04, quantileType = 8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  Vector in which to detect exceptional scores.
}
  \item{prob}{
  Probability that a score is exceptionally positive or negative; i.e. scores
  with a quartile lower than \code{prob} or higher than 1-\code{prob} are
  considered exceptional (if both is TRUE, at least). So, note that a \code{prob}
  of .025 means that if both=TRUE, the most exceptional 5\% of the values is
  marked as such.
}
  \item{both}{
  Whether to consider values exceptional if they're below \code{prob} as well
  as above 1-\code{prob}, or whether to only consider values exceptional if
  they're below \code{prob} is \code{prob} is < .5, or above \code{prob} if
  \code{prob} > .5.
}
  \item{silent}{
  Can be used to suppress messages.
}
  \item{quantileCorrection}{
  By how much to correct the computed quantiles; this is used because when
  a distribution is very right-skewed, the lowest quantile is the lowest
  value, which is then also the mode; without subtracting a correction, almost
  all values would be marked as 'exceptional'.
}
  \item{quantileType}{
  The algorithm used to compute the quantiles; see \code{\link{quantile}}.
}
}
\details{
Note that of course, by definition, \code{prob} of \code{2*prob} percent of the
values is exceptional, so it is usually not a wise idea to remove scores based
on their 'exceptionalness'. Instead, use \code{\link{exceptionalScores}},
which calls this function, to see how often participants answered
exceptionally, and remove them based on that.
}
\value{
A logical vector, indicating for each value in the supplied vector whether it
is exceptional.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{quantile}}, \code{\link{exceptionalScores}}
}
\examples{
exceptionalScore(c(1,1,2,2,2,3,3,3,4,4,4,5,5,5,5,6,6,7,8,20), prob=.05);
}
\keyword{ univariate }
