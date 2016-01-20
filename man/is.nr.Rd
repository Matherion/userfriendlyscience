\name{is.nr}
\alias{is.nr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
is.nr
}
\description{
Convenience function that returns TRUE if the argument is not null, not NA, and is.numeric.
}
\usage{
is.nr(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    The value or vector to check.
}
}
\value{
TRUE or FALSE.
}
\examples{
is.nr(8);    ### Returns TRUE
is.nr(NULL); ### Returns FALSE
is.nr(NA);   ### Returns FALSE
}