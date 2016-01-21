\name{exceptionalScores}
\alias{exceptionalScores}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
exceptionalScores
}
\description{
A function to detect participants that consistently respond exceptionally.
}
\usage{
exceptionalScores(dat, items = NULL, exception = 0.025, totalOnly = TRUE,
                  append = TRUE, both = TRUE, silent = FALSE,
                  suffix = "_isExceptional", totalVarName = "exceptionalScores")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
  The dataframe containing the variables to inspect, or the vector to inspect
  (but for vectors, \code{\link{exceptionalScore}} might be more useful).
}
  \item{items}{
  The names of the variables to inspect.
}
  \item{exception}{
  When an item will be considered exceptional, passed on as \code{prob} to
  \code{\link{exceptionalScore}}.
}
  \item{totalOnly}{
  Whether to return only the number of exceptional scores for each row in the
  dataframe, or for each inspected item, which values are exceptional.
}
  \item{append}{
  Whether to return the supplied dataframe with the new variable(s) appended
  (if TRUE), or whether to only return the new variable(s) (if FALSE).
}
  \item{both}{
  Whether to look for both low and high exceptional scores (TRUE) or not (FALSE;
  see \code{\link{exceptionalScore}}).
}
  \item{silent}{
  Can be used to suppress messages.
}
  \item{suffix}{
  If not returning the total number of exceptional values, for each inspected
  variable, a new variable is returned indicating which values are exceptional.
  The text string is appended to each original variable name to create the new
  variable names.
}
  \item{totalVarName}{
  If returning only the total number of exceptional values, and appending these
  to the provided dataset, this text string is used as variable name.
}
}
\value{
Either a vector containing the number of exceptional values, a dataset
containing, for each inspected variable, which values are exceptional, or the
provided dataset where either the total or the exceptional values for each
variable are appended.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}

\seealso{
\code{\link{exceptionalScore}}
}
\examples{
exceptionalScores(mtcars)
}
