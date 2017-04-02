\name{convert.d.to.nnc}
\alias{convert.d.to.nnc}
\alias{convert.d.to.eer}
\title{
Helper functions for Numbers Needed for Change
}
\description{
These two functions are used by \code{\link{nnc}} to compute the
Numbers Needed for Change.
}
\usage{
convert.d.to.nnc(d, cer, r = 1,
                 eventDesirable = TRUE,
                 eventIfHigher = TRUE)
convert.d.to.eer(d, cer,
                 eventDesirable = TRUE,
                 eventIfHigher = TRUE)
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
  \item{eventDesirable}{
  Whether an event is desirable or undesirable.
}
  \item{eventIfHigher}{
  Whether scores above or below the threshold are considered 'an event'.
}
}
\details{
These two functions are used by \code{\link{nnc}} to compute the
Numbers Needed for Change.
}
\value{
The converted value.
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
convert.d.to.eer(d=.5, cer=.25);
convert.d.to.nnc(d=.5, cer=.25);
}
\keyword{ utilities }
