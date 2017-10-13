\name{nnc}
\alias{nnc}
\title{
Numbers Needed for Change
}
\description{
This function computes the Numbers Needed for Change, and shows a
visualisation to illustrate them.
}
\usage{
nnc(d = NULL, cer = NULL, r = 1, n = NULL,
    threshold = NULL, mean = 0, sd = 1,
    poweredFor = NULL, thresholdSensitivity = NULL,
    eventDesirable = TRUE, eventIfHigher = TRUE,
    conf.level=.95, 
    d.ci = NULL, cer.ci = NULL, r.ci = NULL,
    d.n = NULL, cer.n = NULL, r.n = NULL,
    plot = TRUE, returnPlot = TRUE, silent = FALSE)
}
\arguments{
  \item{d}{
  The value of Cohen's \emph{d}.
}
  \item{cer}{
  The Control Event Rate.
}
  \item{r}{
  The correlation between the determinant and behavior (for mediated
  Numbers Needed for Change).
}
  \item{n}{
  The sample size.
}
  \item{threshold}{
  If the event rate is not available, a threshold value can be specified
  instead, which is then used in conjunction with the mean
  (\code{mean}) and standard deviation (\code{sd}) and assuming
  a normal distribution to compute the event rate.
}
  \item{mean}{
  The mean value, used to draw the plot, or, if no CER is provided but
  instead the threshold value, to compute the CER.
}
  \item{sd}{
  The standard deviation, used to draw the plot (and to compute the CER
  if a threshold value is supplied instead of the CER).
}
  \item{poweredFor}{
  The Cohen's \emph{d} value for which the study was powered. This
  expected Cohen's \emph{d} value can be used to compute the
  threshold, which then in turn is used to compute the CER. To use this
  approach, also specify the mean and the standard deviation.
}
  \item{thresholdSensitivity}{
  This argument can be used to provide a vector of potential threshold
  values, each of which is used to compute an NNC. This enables easy
  inspection of whether the value chosen as threshold matters much for
  the NNC.
}
  \item{eventDesirable}{
  Whether an event is desirable or undesirable.
}
  \item{eventIfHigher}{
  Whether scores above or below the threshold are considered 'an event'.
}
  \item{d.ci}{
  Instead of providing a point estimate for Cohen's \emph{d}, a
  confidence interval can be provided.
}
  \item{cer.ci}{
  Instead of providing a point estimate for the Control Event Rate, a
  confidence interval can be provided.
}
  \item{r.ci}{
  Instead of providing a point estimate for the correlation, a
  confidence interval can be provided.
}
  \item{d.n}{
  In addition to providing a point estimate for Cohen's \emph{d}, a
  sample size can be provided; if it is, the confidence interval is
  computed.
}
  \item{cer.n}{
  In addition to providing a point estimate for the Control Event Rate,
  a sample size can be provided; if it is, the confidence interval is
  computed.
}
  \item{r.n}{
  In addition to providing a point estimate for the correlation, a
  sample size can be provided; if it is, the confidence interval is
  computed.

}
  \item{plot}{
  Whether to generate and show the plot.
}
  \item{returnPlot}{
  Whether to return the plot (as an attribute), or to only display it.
}
  \item{silent}{
  Whether to suppress notifications.
}
}
\details{
  This function computes the Numbers Needed for Change. See
  Gruijters & Peters (2017) for details.
}
\value{
  The Numbers Needed for Change (NNC), potentially with a plot
  visualising the NNC in an attribute.
}
\references{
  Gruijters, S. L. K., & Peters, G.-J. Y. (2017). Introducing the Numbers Needed for Change (NNC): A practical measure of effect size for intervention research.
}
\author{
Gjalt-Jorn Peters & Stefan Gruijters
  
Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
### Simple example
nnc(d=.4, cer=.3);

### Or for a scenario where events are undesirable, and the 
### intervention effective (therefore having a negative value for d):
nnc(d=-.4, cer=.3, eventDesirable=FALSE);
}
\keyword{ utilities }
