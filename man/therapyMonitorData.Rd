\name{therapyMonitorData}
\alias{therapyMonitorData}
\docType{data}
\title{
Data originally published with therapyMonitor
}
\description{
This dataset was originally published along with a Dutch language article that described the \code{\link{therapyMonitor}} function. This version only contains the aggregated scales.
}
\usage{data("therapyMonitorData")}
\format{
  A data frame with 38 observations on the following 12 variables.
  \describe{
    \item{\code{time}}{The measurement moment as stored by Google Forms.}
    \item{\code{datetime}}{The measurement moment converted to POSIXct, R's time format.}
    \item{\code{measurementNumber}}{The rank (number) of each measurement.}
    \item{\code{positiveAffect}}{The positive affect scale.}
    \item{\code{negativeAffect}}{The negative affect scale.}
    \item{\code{selfEsteem}}{A self esteem scale.}
    \item{\code{intimacy}}{An intimacy scale.}
    \item{\code{erectionMasturbation}}{Erection when masturbating.}
    \item{\code{erectionPartnerSex}}{Erection while having sex with partner.}
    \item{\code{experienceMasturbation}}{Experience when masturbating.}
    \item{\code{experiencePartnerSex}}{Experience while having sex with partner.}
    \item{\code{erectionCombined}}{Aggregated scale of both erection experience scales.}
  }
}
\details{
This dataset is an n-of-1 dataset collected during a series of therapy sessions.
}
\source{
van Lankveld, J., Leusink, P., & Peters, G.-J. Y. (2017). Therapie-monitoring in een blended online en face-to-face behandeling van een jonge man met situatieve erectieproblemen. \emph{Tijdschrift voor Seksuologie, 41}, 15–22.

}
\examples{
data(therapyMonitorData)
## maybe str(therapyMonitorData) ; plot(therapyMonitorData) ...
}
\keyword{datasets}
