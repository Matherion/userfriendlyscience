\name{userfriendlyscience-package}
\alias{userfriendlyscience-package}
\alias{userfriendlyscience}
\alias{ufs}
\alias{UFS}
\docType{package}
\title{
Userfriendlyscience (UFS)
}
\description{
This package contains a number of functions that serve
    two goals. First, to make R more accessible to people migrating
    from SPSS by adding a number of functions that behave roughly like
    their SPSS equivalents (also see \url{http://rosettastats.com}). Second,
    to make a number of slightly more advanced functions more user
    friendly to relatively novice users. The package also conveniently
    houses a number of additional functions that are intended to
    increase the quality of methodology and statistics in psychology,
    not by offering technical solutions, but by shifting perspectives,
    for example towards reasoning based on sampling distributions as
    opposed to on point estimates.
}
\details{
\tabular{ll}{
Package: \tab userfriendlyscience\cr
Type: \tab Package\cr
Version: \tab 0.7-1\cr
Date: \tab 2018-05-01\cr
License: \tab GPL (>= 3)\cr
}

Userfriendlyscience (UFS) contains a number of functions that serve two goals.  First, to make R more accessible to people migrating from SPSS by adding a number of functions that behave roughly like their SPSS equivalents (also see \url{http://rosettastats.com} for a tool that helps with this). Second, to make a number of slightly more advanced functions more user friendly to relatively novice users. The package also conveniently houses a number of additional functions that are intended to increase the quality of methodology and statistics in psychology, not by offering technical solutions, but by shifting perspectives, for example towards reasoning based on sampling distributions as opposed to on point estimates.

The package imports functions from many other packages, which is in line with its function as a 'wrapper package': UFS aims to make many existing functions easier for users coming from SPSS, so sometimes a function is added when it saves the user just some data preparing.

The package implements many solutions provided by people all over the world, most from Stack Exchange (both from Cross Validated and Stack Overflow). I credit these authors in the help pages of those functions and in the Author(s) section of this page. If you wrote a function included here, and you want me to take it out, feel free to contact me of course (also, see \url{http://meta.stackoverflow.com/questions/319171/i-would-like-to-use-a-function-written-by-a-stack-overflow-member-in-an-r-packag}).
}
\author{
Author: Gjalt-Jorn Peters (Open University of the Netherlands, Greater Good, and Maastricht University).

Contributors: Peter Verboon (\code{\link{convert.omegasq.to.cohensf}}, \code{\link{genlog}}, and \code{\link{piecewiseRegr}}, Open University of the Netherlands), Amy Chan (\code{\link{ggPie}}), Jeff Baggett (\code{\link{posthocTGH}}, University of Wisconsin - La Crosse), Daniel McNeish (\code{\link{scaleStructure}}, University of North Carolina), Nick Sabbe (\code{\link{curfnfinder}}, Arteveldehogeschool), Douglas Bonett (\code{\link{confIntR}}, \code{\link{pwr.confIntR}}, UC Santa Cruz, United States), Murray Moinester (\code{\link{confIntR}}, \code{\link{pwr.confIntR}}, Tel Aviv University, Israel), Stefan Gruijters (\code{\link{nnc}}, \code{\link{ggNNC}}, \code{\link{convert.d.to.eer}}, \code{\link{convert.d.to.nnc}}, \code{\link{erDataSeq}}, Maastricht University), Ron Pat-El (\code{\link{logRegr}}, Open University of the Netherlands), Ananda Mahto (\code{\link{multiResponse}}).

A number of functions in this package use code fragments that were used without explicit communicating with the author (because I've been unable to find contact details of the authors, or because I haven't gotten around to contacting them yet). The authors of these fragments are John Fox (\code{car} code in \code{\link{ggqq}}), Floo0 (\code{\link{ggqq}}), Jason Aizkalns (\code{\link{ggBoxplot}}), Luke Tierney (in \code{\link{pwr.cohensdCI}}, its alias \code{\link{pwr.confIntd}}, and \code{\link{cohensdCI}}).

In addition, the function \code{escapeRegEx} from package \code{Hmisc} is included and used internally to avoid importing that entire package just for that function. This function was written by Charles Dupont (Department of Biostatistics, Vanderbilt University). The help page was also taken from that package.

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
  \code{\link{psych}} and \code{\link{MBESS}} contain many useful functions
  for researchers in psychology.
}
\references{
Peters, G.-J. Y. (2014). \href{https://ehps.net/ehp/index.php/contents/article/download/ehp.v16.i2.p56/1}{The alpha and the omega of scale reliability and validity: why and how to abandon Cronbach's alpha and the route towards more comprehensive assessment of scale quality.} \emph{European Health Psychologist}, 16(2), 56-69.

Peters, G.-J. Y. (2018). Diamond Plots: a tutorial to introduce a visualisation tool that facilitates interpretation and comparison of multiple sample estimates while respecting their inaccuracy. \emph{PsyArXiv; under review at Health Psychology Bulletin}. Preprint doi: \href{https://doi.org/10.17605/osf.io/9w8yv}{10.17605/osf.io/9w8yv}

Peters, G.-J. Y. & Crutzen, R. (2018). Knowing exactly how effective an intervention, treatment, or manipulation is and ensuring that a study replicates: accuracy in parameter estimation as a partial solution to the replication crisis. \emph{PsyArXiv; under review at Psychology & Health}. Preprint doi: \href{https://doi.org/10.17605/osf.io/cjsk2}{10.17605/osf.io/cjsk2}

Crutzen, R., Peters, G.-J. Y., & Noijen, J. (2018). Using Confidence Interval-Based Estimation of Relevance to Select Social-Cognitive Determinants for Behavior Change Interventions. Frontiers in Public Health 5:165. http://dx.doi.org/10.3389/fpubh.2017.00165

Crutzen, R. (2014). \href{https://ehps.net/ehp/index.php/contents/article/download/ehp.v16.i2.p70/25}{Time is a jailer: what do alpha and its alternatives tell us about reliability?} \emph{The European Health Psychologist}, 1(2), 70-74.

Crutzen, R., & Peters, G.-J. Y. (2015). Scale quality: alpha is an inadequate estimate and factor-analytic evidence is needed first of all. \emph{Health Psychology Review}. doi: \href{http://dx.doi.org/10.1080/17437199.2015.1124240}{10.1080/17437199.2015.1124240}

Verboon, P. & Peters, G.-J. Y. (2018). Applying the generalized logistic model in single case designs. \emph{PsyArXiv}. Preprint doi: \href{https://doi.org/10.17605/osf.io/ad5eh}{10.17605/osf.io/ad5eh}

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

\dontrun{
### Show reliabilities
scaleStructure(dat=exampleData, ci=FALSE,
               omega.psych=FALSE, poly=FALSE);
}

### Create a dichotomous variable
exampleData$group <- cut(exampleData$t0_item2, 2);

### Show item distributions and means
meansDiamondPlot(exampleData);

### Show a dlvPlot
dlvPlot(exampleData, x="group", y="t0_item1");

### show a dlvPlot with less participants, showing the confidence
### interval and standard error bars better
dlvPlot(exampleData[1:30, ], x="group", y="t0_item1");

}
\keyword{ package }
