\name{userfriendlyscience-package}
\alias{userfriendlyscience-package}
\alias{userfriendlyscience}
\docType{package}
\title{
Userfriendlyscience
}
\description{
This package contains a number of functions that serve
  two goals: first, make R more accessible to people migrating from
  SPSS by adding a number of functions that behave roughly like their
  SPSS equivalents; and second, make a number of slightly more
  advanced functions more user friendly to relatively novice users.
}
\details{
\tabular{ll}{
Package: \tab userfriendlyscience\cr
Type: \tab Package\cr
Version: \tab 0.6-0\cr
Date: \tab 2017-04-01\cr
License: \tab GPL (>= 2)\cr
}
The package contains a variety of functions designed to make life easier. These functions are geared towards researchers in psychology.

The package implements many solutions provided by people all over the world, most from Stack Exchange (both from Cross Validated and Stack Overflow). I credit these authors in the help pages of those functions and in the Author(s) section of this page. If you wrote a function included here, and you want me to take it out, feel free to contact me of course (also, see \url{http://meta.stackoverflow.com/questions/319171/i-would-like-to-use-a-function-written-by-a-stack-overflow-member-in-an-r-packag}).
}
\author{
Author: Gjalt-Jorn Peters (Open University of the Netherlands, Greater Good, and Maastricht University).

Contributors: Peter Verboon (\code{\link{convert.omegasq.to.cohensf}}, Open University of the Netherlands), Amy Chan (\code{\link{ggPie}}), Jeff Baggett (\code{\link{posthocTGH}}, University of Wisconsin - La Crosse), Daniel McNeish (\code{\link{scaleStructure}}, University of North Carolina), Nick Sabbe (\code{\link{curfnfinder}}, Arteveldehogeschool), Douglas Bonett (\code{\link{confIntR}}, \code{\link{pwr.confIntR}}, UC Santa Cruz, United States), Murray Moinester (\code{\link{confIntR}}, \code{\link{pwr.confIntR}}, Tel Aviv University, Israel)

A number of functions in this package use code fragments that were used without explicit communicating with the author (because I've been unable to find contact details of the authors, or because I haven't gotten around to contacting them yet). The authors of these fragments are John Fox (\code{\link{car}} code in \code{\link{ggqq}}), Floo0 (\code{\link{ggqq}}), Jason Aizkalns (\code{\link{ggBoxplot}}), Luke Tierney (in \code{\link{pwr.cohensdCI}}, its alias \code{\link{pwr.confIntd}}, and \code{\link{cohensdCI}}).

In addition, the function \code{escapeRegEx} from package \code{Hmisc} is included and used internally to avoid importing that entire package just for that function. This function was written by Charles Dupont (Department of Biostatistics, Vanderbilt University). The help page was also taken from that package.

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
  \code{\link{psych}} and \code{\link{MBESS}} contain many useful functions
  for researchers in psychology.
}
\references{
Peters, G.-J. Y. (2014). The alpha and the omega of scale reliability and validity: why and how to abandon Cronbach's alpha and the route towards more comprehensive assessment of scale quality. \emph{European Health Psychologist}, 16(2), 56-69.
}

\examples{
### Create simple dataset
dat <- PlantGrowth[1:20,];
### Remove third level from group factor
dat$group <- factor(dat$group);

### Examine normality
normalityAssessment(dat$weight);

### Compute mean difference and show it
meanDiff(dat$weight ~ dat$group, plot=TRUE);

### Show the t-test
didacticPlot(meanDiff(dat$weight ~ dat$group)$t,
             statistic='t',
             df1=meanDiff(dat$weight ~ dat$group)$df);

### Load data from simulated dataset testRetestSimData (which
### satisfies essential tau-equivalence).
data(testRetestSimData);

### Select some items in the first measurement
exampleData <- testRetestSimData[2:6];

### Show reliabilities
scaleStructure(dat=exampleData);

### Create a dichotomous variable
exampleData$group <- cut(exampleData$t0_item2, 2);

### Show a dlvPlot
dlvPlot(exampleData, x="group", y="t0_item1");

### show a dlvPlot with less participants, showing the confidence
### interval and standard error bars better
dlvPlot(exampleData[1:30, ], x="group", y="t0_item1");

}
\keyword{ package }
