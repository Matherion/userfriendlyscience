\name{ggProportionPlot}
\alias{ggProportionPlot}
\title{
Sample distribution based plotting of proportions
}
\description{
This function visualises percentages, but avoids a clear cut for the sample point estimate, instead using the confidence (as in confidence interval) to create a gradient. This effectively hinders drawing conclusions on the basis of point estimates, thereby urging caution consistent with what the data allows.
}
\usage{
ggProportionPlot(dat, items,
                 loCategory = min(dat[, items], na.rm = TRUE),
                 hiCategory = max(dat[, items], na.rm = TRUE),
                 subQuestions = NULL,
                 leftAnchors = NULL,
                 rightAnchors = NULL,
                 compareHiToLo = TRUE,
                 showDiamonds = FALSE,
                 diamonds.conf.level=.95,
                 na.rm = TRUE,
                 barHeight = 0.4,
                 scale_color = scale_color_viridis(option = "D",
                                                   begin = 0.25,
                                                   end = 1),
                 theme = theme_bw())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
  The dataframe containing the items (variables).
}
  \item{items}{
  The names of the items (variables).
}
  \item{loCategory}{
  The value of the low category (usually 0). If not provided, the minimum value is used.
}
  \item{hiCategory}{
  The value of the high category (usually 1). If not provided, the maximum value is used.
}
  \item{subQuestions}{
  The labels to use for the variables (for example, different questions). The variable names are used if these aren't provided.
}
  \item{leftAnchors}{
  The labels for the low categories. The values are used if these aren't provided.
}
  \item{rightAnchors}{
  The labels for the high categories. The values are used if these aren't provided.
}
  \item{compareHiToLo}{
  Whether to compare the percentage of low category values to the total of the low category values and the high category values, or whether to ignore the high category values and compute the percentage of low category values relative to all cases. This can be useful when a variable has more than two values, and you only want to know/plot the percentage relative to the total number of cases.
}
  \item{showDiamonds}{
  Whether to add diamonds to illustrate the confidence intervals.
}
  \item{diamonds.conf.level}{
  The confidence level of the diamonds' confidence intervals.
}
  \item{na.rm}{
  Whether to remove missing values.
}
  \item{barHeight}{
  The height of the bars, or rather, half the height. Use .5 to completely fill the space.
}
  \item{scale_color}{
  The function for the colors for the gradient.
}
  \item{theme}{
  The theme to use.
}
}
\details{
This function uses \code{\link{pbeta}}, with some lines of code taken from the \code{\link{binom.test}} source. Specifically, the count for the low category is specified as first 'shape argument' to \code{\link{pbeta}}, and the total count (either the sum of the count for the low category and the count for the high category, or the total number of cases if \code{compareHiToLo} is \code{FALSE}) minus the count for the low category as the second 'shape argument'.
}
\value{
A \code{\link{ggplot2}} object.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{binom.test}}
}
\examples{
### V/S (no idea what this is: ?mtcars only mentions 'V/S' :-))
### and transmission (automatic vs manual)
ggProportionPlot(mtcars, items=c('vs', 'am'));

### Number of cylinders, by default comparing lowest value
### (4) to highest (8):
ggProportionPlot(mtcars, items=c('cyl'));

### We can also compare 4 to 6:
ggProportionPlot(mtcars, items=c('cyl'),
                 hiCategory=6);

### Now compared to total records, instead of to 
### highest value (hiCategory is ignored then)
ggProportionPlot(mtcars, items=c('cyl'),
                 compareHiToLo=FALSE);

### And for 6 cylinders:
ggProportionPlot(mtcars, items=c('cyl'),
                 loCategory=6, compareHiToLo=FALSE);

### And for 8 cylinders:
ggProportionPlot(mtcars, items=c('cyl'),
                 loCategory=8, compareHiToLo=FALSE);

### And for 8 cylinders with different labels
ggProportionPlot(mtcars, items=c('cyl'),
                 loCategory=8,
                 subQuestions='Cylinders',
                 leftAnchors="Eight",
                 rightAnchors="Four\nor\nsix",
                 compareHiToLo=FALSE);

### ... And showing the diamonds for the confidence intervals
ggProportionPlot(mtcars, items=c('cyl'),
                 loCategory=8,
                 subQuestions='Cylinders',
                 leftAnchors="Eight",
                 rightAnchors="Four\nor\nsix",
                 compareHiToLo=FALSE,
                 showDiamonds=TRUE);

}
\keyword{ hplot }
\keyword{ graphs }
