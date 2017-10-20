\name{checkDataIntegrity}
\alias{checkDataIntegrity}
\title{
Conveniently checking data integrity
}
\description{
This function is designed to make it easy to perform some data integrity checks, specifically checking for values that are impossible or unrealistic. These values can then be replaced by another value, or the offending cases can be deleted from the dataframe.
}
\usage{
checkDataIntegrity(x,
                   dat,
                   newValue = NA,
                   removeCases = FALSE,
                   validValueSuffix = "_validValue",
                   newValueSuffix = "_newValue",
                   totalVarName = "numberOfInvalidValues",
                   append = TRUE,
                   replace = TRUE,
                   silent = FALSE,
                   rmarkdownOutput=FALSE,
                   callingSelf = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  This can be either a vector or a list. If it is a vector, it should have two elements, the first one being a regular expression matching one or more variables in the dataframe specified in \code{dat}, and second one being the condition the matching variables have to satisfy. If it is a list, it should be a list of such vectors. The conditions should start with a \code{\link{Comparison}} operator followed by a value (e.g. "<30" or ">=0).
}
  \item{dat}{
  The dataframe containing the variables of which we should check the integrity.
}
  \item{newValue}{
  The new value to be assigned to cases not satisfying the specified conditions.
}
  \item{removeCases}{
  Whether to delete cases that do not satisfy the criterion from the dataframe (if \code{FALSE}, they're not deleted, but the offending value is replaced by \code{newValue}).
}
  \item{validValueSuffix}{
  Suffix to append to variable names when creating variable names for new variables that contain TRUE and FALSE to specify for each original variable whether its value satisfied the specified criterion.
}
  \item{newValueSuffix}{
  If \code{replace} is \code{FALSE}, original values are not replaced, but instead new variables are created where the offending values have been replaced. This suffix is appended to each original variable name to create the new variable name.
}
  \item{totalVarName}{
  This is the name of a variable that contains, for each case, the total number of invalid values among all variables checked.
}
  \item{append}{
  Whether to append the columns to the dataframe, or only return the new columns.
}
  \item{replace}{
  Whether to replace the offending values with the value specified in \code{newValue} or whether to create new columns (see \code{newValueSuffix}).
}
  \item{silent}{
  Whether to display the log, or only set it as attribute of the returned dataframe.
}
  \item{rmarkdownOutput}{
  Whether to format the log so that it's ready to be included in RMarkdown reports.
}
  \item{callingSelf}{
  For internal use; whether the function calls itself.
}
}
\value{
The dataframe with the corrections, and the log stored in attribute \code{checkDataIntegrity_log}.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
### Default behavior: return dataframe with
### offending values replaced by NA

checkDataIntegrity(c('mpg', '<30'),
                   mtcars);

### Check two conditions, and instead of returning the
### dataframe with the results appended, only return the
### columns indicating which cases 'pass', what the new
### values would be, and how many invalid values were
### found for each case (to easily remove cases that
### provided many invalid values)

checkDataIntegrity(list(c('mpg', '<30'),
                        c('gear', '<5')),
                   mtcars,
                   append=FALSE);
}
\keyword{ utilities }
