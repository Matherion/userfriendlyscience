\name{showPearsonPower}
\alias{showPearsonPower}
\title{
Visualisation of the power of a Pearson correlation test
}
\description{
This fuction is useful when conducting power analyses for a Pearson correlation. It draws the sampling distribution of Pearson's \emph{r} assuming a null hypothesis value of \emph{r} and assuming a the hypothetical population value. The probability of making a Type 1 error is also illustrated.
}
\usage{
showPearsonPower(n = 100, rho = 0.3, rNull = 0,
                 distLabels = c("Null Hypothesis", "Population"),
                 rhoColor = "green", rhoFill = "green",
                 rhoAlpha = 0.1, rhoLineSize = 1,
                 rNullColor = "blue", rNullFill = "blue",
                 rNullAlpha = 0.1, rNullLineSize = 1,
                 type2Color = "red", type2Fill = "red",
                 type2Alpha = 0.1, type2LineSize = 0,
                 theme = dlvTheme(), alpha = 0.05, digits = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
  The number of participants.
}
  \item{rho}{
  The value of the correlation coefficient in the population.
}
  \item{rNull}{
  The value of the correlation coefficient according to the null hypothesis.
}
  \item{distLabels}{
  Labels for the two distributions; the first one is the null hypothesis distribution, the second one the alternative distribution.
}
  \item{rhoColor, rNullColor, type2Color}{
  The border colors of the distributions and the region used to illustrate the Type 2 error probability.
}
  \item{rhoFill, rNullFill, type2Fill}{
  The fill colors of the distributions and the region used to illustrate the Type 2 error probability.
}
  \item{rhoAlpha, rNullAlpha, type2Alpha}{
  The alpha (transparency) of the distributions and the region used to illustrate the Type 2 error probability.
}
  \item{rhoLineSize, rNullLineSize, type2LineSize}{
  The line thicknesses of the distributions and the region used to illustrate the Type 2 error probability.
}
  \item{theme}{
  The theme to use.
}
  \item{alpha}{
  The significance level (alpha) of the null hypothesis test.
}
  \item{digits}{
  The number of digits to round to.
}
}
\value{
A \code{\link{ggplot}} plot is returned.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{didacticPlot}}
}
\examples{
\dontrun{
showPearsonPower();
}
}
\keyword{ hplot }
