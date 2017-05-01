\name{ggConfidenceCurve}
\alias{ggConfidenceCurve}
\title{
Confidence Curves
}
\description{
Confidence curves are a way to show the confidence in an estimate computed from sample data. They are useful because they show all confidence levels simultaneously, thereby giving a good sense of the accuracy of the estimate, without forcing the researchers to make a more or less arbitrary choice for one confidence level.
}
\usage{
ggConfidenceCurve(metric = "d",
                  value = NULL,
                  n = NULL,
                  conf.level = NULL,
                  wRange = c(0.05, 0.8),
                  curveSize = 1,
                  curveColor = "black",
                  confRange = c(1e-04, 0.9999),
                  confLines = c(0.5, 0.8, 0.95, 0.99),
                  confLineColor = "grey",
                  confLineSize = 1,
                  xlab = metric,
                  steps = 1000,
                  theme = theme_bw())
}
\arguments{
  \item{metric}{
  The metric, currently only 'd' (Cohen's \emph{d}) and 'r' (Pearson's \emph{r}) are implemented.
}
  \item{value}{
  The value for which to create the confidence curve plot.
}
  \item{n}{
  The sample size for which to create the confidence curve plot. If \code{n} is specified, the y axis shows confidence levels (i.e. a conventional confidence curve is generated). If \code{n} is set to \code{NULL}, the y axis shows sample sizes. Either \code{n} or \code{conf.level} must be \code{NULL}.
}
  \item{conf.level}{
  The confidence level for which to create the confidence curve plot. If \code{conf.level} is specified, the y axis shows sample sizes. If \code{conf.level} is set to \code{NULL}, the y axis shows confidence levels (i.e. a conventional confidence curve is generated). Either \code{n} or \code{conf.level} must be \code{NULL}.
}
  \item{wRange}{
  The range of 'half-widths', or margins of error, to plot in the confidence curve plot if no sample size is specified (if \code{n=NULL}).
}
  \item{curveSize}{
  The line size of the confidence curve line.
}
  \item{curveColor}{
  The color of the confidence curve line.
}
  \item{confRange}{
  The range of confidence levels to plot.
}
  \item{confLines}{
  If a traditional confidence curve is generated, lines can be added to indicate the metric values corresponding to the lower and upper confidence interval bounds.
}
  \item{confLineColor}{
  If confidence lines are added (see \code{confLines}), this is the color in which they are drawn.
}
  \item{confLineSize}{
  If confidence lines are added (see \code{confLines}), this is the size in which they are drawn.
}
  \item{xlab}{
  The label on the x axis.
}
  \item{steps}{
  The number of steps to use when generating the data for the confidence curves' more steps yield prettier, smoother curves, but take more time.
}
  \item{theme}{
  The \code{\link{ggplot}} theme to use.
}
}
\value{
  A \code{\link{ggplot2}} plot.
}
\references{
Bender, R., Berg, G., & Zeeb, H. (2005). Tutorial: Using confidence curves in medical research. \emph{Biometrical Journal}, 47(2), 237-247. http://doi.org/10.1002/bimj.200410104

Birnbaum, A. (1961). Confidence curves: An omnibus technique for estimation and testing statistical hypotheses. \emph{Journal of the American Statistical Association}, 56(294), 246-249. http://doi.org/10.1080/01621459.1961.10482107
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{cohensdCI}}, \code{\link{pwr.cohensdCI}}, \code{\link{confIntR}}, \code{\link{pwr.confIntR}}
}
\examples{
ggConfidenceCurve(metric='d', value = .5, n = 128);
ggConfidenceCurve(metric='d', value = .5, conf.level = .95);
}
\keyword{ hplot }
