\name{ggNNC}
\alias{ggNNC}
\alias{erDataSeq}
\title{
Visualising Numbers Needed for Change
}
\description{
These functions can be used to visualise Numbers Needed for Change. \code{erDataSeq} is a helper function to generate an Event Rate Data Sequence.
}
\usage{
erDataSeq(er = NULL, erValue = NULL,
          meanValue = NULL, sd = NULL,
          eventIfHigher = TRUE,
          pRange = c(1e-06, 0.99999),
          xStep = 0.01)

ggNNC(cerDataSeq, d = NULL, eventDesirable = TRUE,
      r = 1, xlab = "Continuous outcome",
      plotTitle = c("Numbers Needed for Change = ", ""),
      theme = theme_bw(), lineSize = 1,
      cerColor = "#EBF2F8", eerColor = "#172F47",
      cerLineColor = "#888888", eerLineColor = "#000000",
      dArrowColor = "#000000", cerAlpha = 0.66,
      eerAlpha = 0.66, xLim = NULL,
      xLimAutoDensityTolerance = 0.001,
      showLegend = TRUE, verticalLineColor = "#172F47",
      desirableColor = "#00FF00", desirableAlpha = 0.2,
      undesirableColor = "#FF0000", undesirableAlpha = 0.2,
      desirableTextColor = "#009900",
      undesirableTextColor = "#990000",
      dArrowDistance = 0.04 * max(cerDataSeq$density),
      dLabelDistance = 0.08 * max(cerDataSeq$density))
}

\arguments{
  \item{er}{
  Event rate to visualise.
}
  \item{erValue}{
  If the event rate is not available, a threshold value can be specified
  instead, which is then used in conjunction with the mean
  (\code{meanValue}) and standard deviation (\code{sd}) and assuming
  a normal distribution to compute the event rate.
  
}
  \item{meanValue}{
  The mean of the control group distribution.
}
  \item{sd}{
  The standard deviation (of the control distribution, but assumed to 
  be the same for both distributions).
}
  \item{eventIfHigher}{
  Whether scores above or below the threshold are considered 'an event'.
}
  \item{pRange}{
  The range of probabilities for which to so the distribution.
}
  \item{xStep}{
  Precision of the drawn distribution; higher values mean lower
  precision/granularity/resolution.
}
  \item{cerDataSeq}{
  The \code{cerDataSeq} object.
}
  \item{d}{
  The value of Cohen's \emph{d}.
}
  \item{eventDesirable}{
  Whether an event is desirable or undesirable.
}
  \item{r}{
  The correlation between the determinant and behavior (for mediated NNC's).
}
  \item{xlab}{
  The label to display for the X axis.
}
  \item{plotTitle}{
  The title of the plot; either one character value, this value if
  used; if two, they are considered a prefix and suffix to be pre/appended
  to the NNC value.
}
  \item{theme}{
  The theme to use for the plot.
}
  \item{lineSize}{
  The thickness of the lines in the plot.
}
  \item{cerColor}{
  The color to use for the event rate portion of the control
  group distribution.
}
  \item{eerColor}{
  The color to use for the event rate portion of the experimental
  group distribution.
}
  \item{cerLineColor}{
  The line color to use for the control group distribution.
}
  \item{eerLineColor}{
  The line color to use for the experimental group distribution.
}
  \item{dArrowColor}{
  The color of the arrow to show the effect size.
}
  \item{cerAlpha}{
  The alpha value (transparency) to use for the control group distribution.
}
  \item{eerAlpha}{
  The alpha value (transparency) to use for the control group distribution.
}
  \item{xLim}{
  This can be used to manually specify the limits for the X axis; if
  \code{NULL}, sensible limits will be derived using \code{xLimAutoDensityTolerance}.
}
  \item{xLimAutoDensityTolerance}{
  If \code{xLim} is \code{NULL}, the limits will be set where the density
  falls below this proportion of its maximum value.
}
  \item{showLegend}{
  Whether to show the legend (only if showing two distributions).
}
  \item{verticalLineColor}{
  The color of the vertical line used to indicate the threshold.
}
  \item{desirableColor}{
  The color for the desirable portion of the X axis.
}
  \item{desirableAlpha}{
  The alpha for the desirable portion of the X axis.
}
  \item{undesirableColor}{
  The color for the undesirable portion of the X axis.
}
  \item{undesirableAlpha}{
  The color for the undesirable portion of the X axis.
}
  \item{desirableTextColor}{
  The color for the text to indicate the desirable portion of the X axis.
}
  \item{undesirableTextColor}{
  The color for the text to indicate the undesirable portion of the X axis.
}
  \item{dArrowDistance}{
  The distance of the effect size arrow from the top of the distributions.
}
  \item{dLabelDistance}{
  The distance of the effect size label from the top of the distributions.
}
}
\details{
  These functions are used by \code{\link{nnc}} to show the distributions,
  and event rates. They probably won't be used much on their own.
}
\value{
  \code{erDataSeq} returns a data sequence; \code{ggNNC} a
  \code{\link{ggplot}}.
}
\references{
  Gruijters, S. L. K., & Peters, G.-J. Y. (2017). Introducing the Numbers Needed for Change (NNC): A practical measure of effect size for intervention research.
}
\author{
Gjalt-Jorn Peters & Stefan Gruijters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{nnc}}
}
\examples{
### Show distribution for an event rate value of 125
ggNNC(erDataSeq(erValue=125, meanValue=90, sd=30));

### If the event occurs under the threshold instead of
### above it
ggNNC(erDataSeq(erValue=125, meanValue=90, sd=30,
      eventIfHigher = FALSE));

### ... And for undesirable events (note how
### desirability is an argument for ggNNC, whereas
### whether an event occurs 'above' or 'below' the
### threshold is an argument for erDataSeq):
ggNNC(erDataSeq(erValue=125, meanValue=90, sd=30,
                eventIfHigher = FALSE),
      eventDesirable = FALSE);

### Show event rate for both experimental and
### control conditions, and show the numbers
### needed for change
ggNNC(erDataSeq(erValue=125, meanValue=90, sd=30), d=.5);

### Illustration of how even with very large effect
### sizes, if the control event rate is very high,
### you'll still need a high number of NNC
ggNNC(erDataSeq(er=.9), d=1);

}
\keyword{ utilities }
