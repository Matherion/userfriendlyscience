\name{genlogFunction}
\alias{genlogFunction}
\title{
Generalized Logistic Function
}
\description{
This is the core function of the generalized logistic analysis used in \code{\link{genlog}}.
}
\usage{
genlogFunction(x, x0, Ab, At, B, v)
}
\arguments{
  \item{x}{
  A numeric vector with measurement moments or indices of measurement moments.
}
  \item{x0}{
  A single numeric value specifying at which moment the curve is at its midpoint (when \code{v} = 1).
}
  \item{Ab, At}{
  Respectively the lowest and highest possible values of the dependent variable.
}
  \item{B}{
  The growth rate (curve steepness).
}
  \item{v}{
  Um - Peter, wat is 'v' eigenlijk?
}
}
\details{
For details, see Verboon & Peters (2017).
}
\references{
Verboon, P. & Peters, G.-J. Y. (2017) Applying the generalised logistic model in SCD to deal with ceiling effects. \emph{PsyArXiv} http://INSERTLINK
}
\author{
Peter Verboon (Open University of the Netherlands)

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{genlog}}
}
\examples{
time <- 1:20;
yVar <- genlogFunction(1:20, 10, 1, 7, 1, 1);
plot(time, yVar, type='l', xlab='time', ylab='y');
}
\keyword{ utilities }
