\name{ggProportionPlot}
\alias{ggProportionPlot}
\title{
Sample distribution based plotting of proportions
}
\description{
This function visualises percentages, but avoids a clear cut for the sample point estimate, instead using the confidence (as in confidence interval) to create a gradient. This effectively hinders drawing conclusions on the basis of point estimates, thereby urging a level of caution that is consistent with what the data allows.
}
\usage{
ggProportionPlot(dat, items = NULL,
                 loCategory = NULL,
                 hiCategory = NULL,
                 subQuestions = NULL,
                 leftAnchors = NULL,
                 rightAnchors = NULL,
                 compareHiToLo = TRUE,
                 showDiamonds = FALSE,
                 diamonds.conf.level=.95,
                 diamonds.alpha=1,
                 na.rm = TRUE,
                 barHeight = 0.4,
                 conf.steps = seq(from=0.001, to=.999, by=.001),
                 scale_color = viridis(option="magma", 2, begin=0, end=.5),
                 scale_fill = viridis(option="magma", 2, begin=0, end=.5),
                 linetype=1,
                 theme = theme_bw(),
                 returnPlotOnly = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
  The dataframe containing the items (variables), or a vector.
}
  \item{items}{
  The names of the items (variables). If none are specified, all variables in the dataframe are used.
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
  \item{diamonds.alpha}{
  The alpha channel (i.e. transparency, or rather 'obliqueness') of the diamonds.
}
  \item{na.rm}{
  Whether to remove missing values.
}
  \item{barHeight}{
  The height of the bars, or rather, half the height. Use .5 to completely fill the space.
}
  \item{conf.steps}{
  The number of steps to use to generate the confidence levels for the proportion.
}
  \item{scale_color, scale_fill}{
  A vector with two values (valid colors), that are used for the colors (stroke) and fill for the gradient; both vectors should normally be the same, but if you feel adventurous, you can play around with the number of \code{conf.steps} and this. If you specify only one color, no gradient is used but a single color (i.e. specifying the same single color for both \code{scale_color} and \code{scale_fill} simply draws bars of that color).}
  \item{linetype}{
  The \code{\link{linetype}} to use (0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash).
}
  \item{theme}{
  The theme to use.
}
  \item{returnPlotOnly}{
  Whether to only return the \code{\link{ggplot2}} plot or the full object including intermediate values and objects.
}
}
\details{
This function used \code{\link{confIntProp}} to compute confidence intervals for proportions at different levels of confidence. The confidence interval bounds at those levels of confidence are then used to draw rectangles with colors in a gradient that corresponds to the confidence level.

Note that percentually, the gradient may not look continuous because at the borders between lighter and darker rectangles, the shade of the lighter rectangle is perceived as even lighter than it is, and the shade of the darker rectangle is perceived as even darker. This makes it seem as if each rectange is coloured with a gradient in the opposite direction.
}
\value{
A \code{\link{ggplot2}} object (if \code{returnPlotOnly} is TRUE), or an object containing that \code{\link{ggplot2}} object and intermediate products.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{confIntProp}} and \code{\link{binom.test}}
}
\examples{
### V/S (no idea what this is: ?mtcars only mentions 'V/S' :-))
### and transmission (automatic vs manual)
ggProportionPlot(mtcars, items=c('vs', 'am'));

### Number of cylinders, by default comparing lowest value
### (4) to highest (8):
ggProportionPlot(mtcars, items=c('cyl'));

\dontrun{
### Not running these to save time during package building/checking

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

### Using less steps for the confidence levels and changing
### the fill colours
ggProportionPlot(mtcars,
                 items=c('vs', 'am'),
                 showDiamonds = TRUE,
                 scale_fill = c("#B63679FF", "#FCFDBFFF"),
                 conf.steps=seq(from=0.0001, to=.9999, by=.2));

}
\keyword{ hplot }
\keyword{ graphs }
