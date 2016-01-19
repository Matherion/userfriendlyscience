\name{userfriendlyscience-package}
\alias{userfriendlyscience-package}
\alias{userfriendlyscience}
\alias{addToLog}
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
Version: \tab 0.4-0\cr
Date: \tab 2015-\cr
License: \tab GPL (>= 2)\cr
}
The package contains a variety of functions designed to make life easier. These functions are geared towards researchers in psychology.
}
\author{
Gjalt-Jorn Peters

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