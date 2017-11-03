\name{multiResponse}
\alias{multiResponse}
\title{
Generate a table for multiple response questions
}
\description{
This function is based on the excellent and extensive Stack Exchange answer by Ananda Mahto at https://stackoverflow.com/questions/9265003/analysis-of-multiple-response. The function basically mimics the behavior of the table produced by SPSS for multiple response questions.
}
\usage{
multiResponse(data,
              items = NULL,
              regex = NULL,
              endorsedOption = 1)
}
\arguments{
  \item{data}{
  Dataframe containing the variables to display.
}
  \item{items, regex}{
  Arguments \code{items} and \code{regex} can be used to specify which variables to process. \code{items} should contain the variable (column) names (or indices), and \code{regex} should contain a regular expression used to match to the column names of the dataframe. If none is provided, all variables in the dataframe are processed.
}
  \item{endorsedOption}{
  Which value represents the endorsed option (note that producing this kind of table requires dichotomous items, where each variable is either endorsed or not endorsed, so this is also a way to treat other variables as dichotomour).
}
}
\value{
A dataframe with columns \code{Option}, \code{Frequency}, \code{Percentage}, and \code{Percentage of (X) cases}, where X is the number of cases.
}
\author{
Ananda Mahto; implemented in this package (and tweaked a bit) by Gjalt-Jorn Peters.

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
multiResponse(mtcars, c('vs', 'am'));
}
\keyword{ utilities }
