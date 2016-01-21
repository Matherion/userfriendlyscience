\name{posthocTGH}
\alias{posthocTGH}
\title{
posthocTGH
}
\description{
This function is used by the 'oneway' function for oneway analysis of
variance in case a user requests post-hoc tests using the Tukey or
Games-Howell methods.
}
\usage{
posthocTGH(y, x, method=c("games-howell", "tukey"), digits=2)
}
\arguments{
  \item{y}{
y has to be a numeric vector.
}
  \item{x}{
x has to be vector that either is a factor or can be converted into one.
}
  \item{method}{
Which post-hoc tests to conduct. Valid values are "tukey" and "games-howell".
}
  \item{digits}{
The number of digits to show in the output.
}
}
\value{
A list of three elements:
\item{input}{List with input arguments}
\item{intermediate}{List of intermediate objects.}
\item{output}{List with two objects 'tukey' and 'games.howell', containing
the outcomes for the respective post-hoc tests.}
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}

\examples{
### Compute post-hoc statistics using the tukey method
posthocTGH(y=ChickWeight$weight, x=ChickWeight$Diet, method="tukey");
### Compute post-hoc statistics using the games-howell method
posthocTGH(y=ChickWeight$weight, x=ChickWeight$Diet);
}
\keyword{ utilities }