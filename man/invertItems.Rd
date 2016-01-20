\name{invertItems}
\alias{invertItems}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
invertItems
}
\description{
Inverts items (as in, in a questionnaire), by calling \code{\link{invertItem}}
on all relevant items.
}
\usage{
invertItems(dat, items = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
  The dataframe containing the variables to invert.
}
  \item{items}{
  The names or indices of the variables to invert. If not supplied (i.e. NULL),
  all variables in the dataframe will be inverted.
}
  \item{\dots}{
  Arguments (parameters) passed on to data.frame when recreating that after
  having used lapply.
}
}
\value{
The dataframe with the specified items inverted.
}
\seealso{
\code{\link{invertItem}}
}
\examples{
invertItems(mtcars, c('cyl'));
}

\keyword{ univariate }
