\name{removeExceptionalValues}
\alias{removeExceptionalValues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
removeExceptionalValues
}
\description{
A function to replace exceptional values with NA. This can be used to quickly
remove impossible values, for example, when participants entered their age as
344.
}
\usage{
removeExceptionalValues(dat, items = NULL, exception = 0.005,
                        silent = FALSE, stringsAsFactors = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
  The dataframe containing the items to inspect.
}
  \item{items}{
  The items to inspect.
}
  \item{exception}{
  How rare a value must be to be considered exceptional (and replaced by NA).
}
  \item{silent}{
  Can be used to suppress messages.
}
  \item{stringsAsFactors}{
  Whether to convert strings to factors when creating a dataframe from lapply
  output.
}
}
\details{
Note that exceptional values may be errors (e.g. participants accidently
pressed a key twice, or during data entry, something went wrong), but they may
also be indicative of participants who did not seriously participate in the
study. Therefore, it is advised to first use \code{\link{exceptionalScores}} to
look for patterns where participants enter many exceptional scores.
}
\value{
The dataframe, with exceptional values replaced by NA.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{exceptionalScores}}
}
\examples{
removeExceptionalValues(mtcars, exception=.1);
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }