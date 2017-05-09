\name{processLimeSurveyDropouts}
\alias{processLimeSurveyDropouts}
\title{
processLimeSurveyDropouts
}
\description{
This function makes it easy to parse the dropouts from a LimeSurvey questionnaire.
}
\usage{
processLimeSurveyDropouts(lastpage,
                          pagenames = NULL,
                          relevantPagenames = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lastpage}{
  A vector with the 'lastpage' variable as LimeSurvey stores it (an integer denoting the last page a participant visited, in other words, where they dropped out).
}
  \item{pagenames}{
  Optional: names for each page.
}
  \item{relevantPagenames}{
  Optional: the names of those pages that should be included.
}
}
\details{
This will be described more in detail in a forthcoming publications.
}
\value{
A list with information about the dropout, including \code{\link{ggplot}}s.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
processLimeSurveyDropouts(c(1,2,1,1,2,3,2,2,3,2,1));
}
\keyword{ untilities }
