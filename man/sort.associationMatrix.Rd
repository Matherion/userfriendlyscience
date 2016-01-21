\name{sort.associationMatrix}
\alias{sort.associationMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
sort.associationMatrix
}
\description{
This function sorts an \code{\link{associationMatrix}} ascendingly or descendingly by one
of its columns.
}
\usage{
\method{sort}{associationMatrix}(x, decreasing = TRUE, byColumn = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  The \code{\link{associationMatrix}} object to sort.
}
  \item{decreasing}{
  Whether to sort ascendingly (FALSE) or descending (TRUE).
}
  \item{byColumn}{
  Which column to sort the matrix by, as an index.
}
  \item{\dots}{
  Passed on to \code{\link{sort}}.
}
}
\details{
Note that if the \code{\link{associationMatrix}} contains values of different effectsizes,
the sorting may be misleading. For example, a value of Cohen's d of .45 is higher
than a value of Pearson's r of .35, and so will end up higher in a 'decreasing'
sort - even though the association represented by an r of .35 is stronger than
that represented by a d of .45.

Furthermore, only asymmetrical associationMatrices can be sorted; sorting a
symmetrical association matrix would also change the order of the columns, after
all.
}
\value{
The \code{\link{associationMatrix}}, but sorted.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{associationMatrix}}
}
\examples{
sort(associationMatrix(infert, y=c("parity", "age"),
                       x=c("induced", "case", "spontaneous"), colNames=TRUE));
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
