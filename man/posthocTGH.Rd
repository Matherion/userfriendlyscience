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
posthocTGH(y, x, method=c("games-howell", "tukey"),
           conf.level = 0.95, digits=2, p.adjust="holm",
           formatPvalue = TRUE)
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
  \item{conf.level}{
Confidence level of the confidence intervals.
}
  \item{digits}{
The number of digits to show in the output.
}
  \item{p.adjust}{
  Any valid \code{\link{p.adjust}} method.
}
  \item{formatPvalue}{
  Whether to format the p values according to APA standards (i.e. replace all values lower than .001 with '<.001'). This only applies to the printing of the object, not to the way the p values are stored in the object.
  }
}
\value{
A list of three elements:
\item{input}{List with input arguments}
\item{intermediate}{List of intermediate objects.}
\item{output}{List with two objects 'tukey' and 'games.howell', containing
the outcomes for the respective post-hoc tests.}
}
\note{
This function is based on a file that was once hosted at http://www.psych.yorku.ca/cribbie/6130/games_howell.R, but has been removed since. It was then adjusted for implementation in the \code{\link{userfriendlyscience}} package. Jeffrey Baggett needed the confidence intervals, and so emailed them, after which his updated functions was used. In the meantime, it appears Aaron Schlegel (\url{https://rpubs.com/aaronsc32}) independently developed a version with confidence intervals and posted it on RPubs at \url{https://rpubs.com/aaronsc32/games-howell-test}.
}
\author{
Gjalt-Jorn Peters (Open University of the Netherlands) & Jeff Bagget (University of Wisconsin - La Crosse)

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}

\examples{
### Compute post-hoc statistics using the tukey method
posthocTGH(y=ChickWeight$weight, x=ChickWeight$Diet, method="tukey");
### Compute post-hoc statistics using the games-howell method
posthocTGH(y=ChickWeight$weight, x=ChickWeight$Diet);
}
\keyword{ utilities }
