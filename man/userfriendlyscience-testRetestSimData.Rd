\name{testRetestSimData}
\alias{testRetestSimData}
\docType{data}
\title{
testRetestSimData is a simulated dataframe used to demonstrate the testRetestAlpha coefficient function.
}
\description{
This dataset contains the true scores of 250 participants on some variable, and 10 items of a scale administered twice (at t0 and at t1).
}
\usage{data(testRetestSimData)}
\format{
  A data frame with 250 observations on the following 21 variables.
  \describe{
    \item{\code{trueScore}}{The true scores}
    \item{\code{t0_item1}}{Score on item 1 at test}
    \item{\code{t0_item2}}{Score on item 2 at test}
    \item{\code{t0_item3}}{Score on item 3 at test}
    \item{\code{t0_item4}}{Score on item 4 at test}
    \item{\code{t0_item5}}{Score on item 5 at test}
    \item{\code{t0_item6}}{Score on item 6 at test}
    \item{\code{t0_item7}}{Score on item 7 at test}
    \item{\code{t0_item8}}{Score on item 8 at test}
    \item{\code{t0_item9}}{Score on item 9 at test}
    \item{\code{t0_item10}}{Score on item 10 at test}
    \item{\code{t1_item1}}{Score on item 1 at retest}
    \item{\code{t1_item2}}{Score on item 2 at retest}
    \item{\code{t1_item3}}{Score on item 3 at retest}
    \item{\code{t1_item4}}{Score on item 4 at retest}
    \item{\code{t1_item5}}{Score on item 5 at retest}
    \item{\code{t1_item6}}{Score on item 6 at retest}
    \item{\code{t1_item7}}{Score on item 7 at retest}
    \item{\code{t1_item8}}{Score on item 8 at retest}
    \item{\code{t1_item9}}{Score on item 9 at retest}
    \item{\code{t1_item10}}{Score on item 10 at retest}
  }
}
\details{
This dataset was generated with the code in the reliabilityTest.r test script.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
data(testRetestSimData);
head(testRetestSimData);
hist(testRetestSimData$t0_item1);
cor(testRetestSimData);
}
\keyword{datasets}
