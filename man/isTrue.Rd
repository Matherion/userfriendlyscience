\name{isTrue}
\alias{isTrue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
isTrue
}
\description{
Returns TRUE for TRUE elements, FALSE for FALSE elements, and whatever is
specified in na for NA items.
}
\usage{
isTrue(x, na = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The vector to check for TRUE, FALSE, and NA values.
}
  \item{na}{
What to return for NA values.
}
}
\value{
A logical vector.
}
\examples{
isTrue(c(TRUE, FALSE, NA));  
isTrue(c(TRUE, FALSE, NA), na=TRUE);
}
\keyword{ univariate }