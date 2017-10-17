\name{Singh}
\alias{Singh}
\docType{data}
\title{
Verbal and physical aggression scores from Singh et al. (2007)
}
\description{
This is a dataset originally described in Singh et al. (2007), and digitized by Rumen Manolov using plot digitizer software and used to illustrate a number of single case design analysis approaches in Manolov & Moeyaert (2016). It is also used by Verboon & Peters (2017) to illustrate the \code{\link{piecewiseRegr}} and the \code{\link{genlog}} functions.
}
\usage{data("Singh")}
\format{
  A data frame with 56 observations on the following 6 variables.
  \describe{
    \item{\code{tier}}{A numeric subject identifier.}
    \item{\code{id}}{A character subject identifier (i.e. a name).}
    \item{\code{time}}{An index of the measurement moment.}
    \item{\code{phase}}{A dummy variable indicating the phase of the experiment: 0 means that treatment has not yet started, 1 means that treatment has started.}
    \item{\code{score_physical}}{The subjects' scores on physical aggression.}
    \item{\code{score_verbal}}{The subjects' scores on verbal aggression.}
  }
}
\source{
https://www.dropbox.com/s/95ptt7atf45jfvb/SinghData.xlsx?dl=0 (also see https://www.dropbox.com/s/bocsaj8yvnrlyvq/Tutorial.pdf?dl=0 for the accompanying tutorial)
}
\references{
Singh, N. N., Lancioni, G. E., Winton, A. S., Adkins, A. D., Wahler, R. G., Sabaawi, M., & Singh, J. (2007). Individuals with mental illness can control their aggressive behavior through mindfulness training. \emph{Behavior Modification, 31}(3), 313-328. http://doi.org/10.1177/0145445506293585

Manolov, R., & Moeyaert, M. (2017). How Can Single-Case Data Be Analyzed? Software Resources, Tutorial, and Reflections on Analysis. \emph{Behavior Modification, 41}(2), 179-228. http://doi.org/10.1177/0145445516664307

Verboon, P. & Peters, G.-J. Y. (2017) Applying the generalised logistic model in SCD to deal with ceiling effects. \emph{PsyArXiv} http://INSERTLINK
}
\seealso{
\code{\link{piecewiseRegr}} and \code{\link{genlog}} both contain examples using this dataset.
}
\examples{
### To load the data, use:
data(Singh);
}
\keyword{datasets}
