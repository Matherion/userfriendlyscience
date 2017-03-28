\name{curfnfinder}
\alias{curfnfinder}
\title{
Function to find the name of the calling function
}
\description{
This function finds and returns the name of the function calling it. This
can be useful, for example, when generating functions algorithmically.
}
\usage{
curfnfinder(skipframes = 0, skipnames = "(FUN)|(.+apply)|(replicate)",
            retIfNone = "Not in function", retStack = FALSE,
            extraPrefPerLevel = "\t")
}
\arguments{
  \item{skipframes}{
  Number of frames to skip; useful when called from an anonymous function.
}
  \item{skipnames}{
  A regular expression specifying which substrings to delete.
}
  \item{retIfNone}{
  What to return when called from outside a function.
}
  \item{retStack}{
  Whether to return the entire stack or just one function.
}
  \item{extraPrefPerLevel}{
  Extra prefixes to return for each level of the function.
}
}
\details{
  This function was written by Nick Sabbe for his package \code{addendum}.
  He posted it on Stack Exchange at
  \url{http://stackoverflow.com/questions/7307987/logging-current-function-name}
  and I included this here with this permission.
}
\value{
The current function.
}
\author{
Nick Sabbe (Arteveldehogeschool)

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
  functionA <- functionB <- function() {
    curFn <- curfnfinder();
    if (curFn == 'functionA') {
      cat('Doing something\n');
    } else {
      cat('Doing something else\n');
    }
    cat('Doing something generic.');
  }
  functionA();
  functionB();
}
\keyword{ utility }
