\name{processLSvarLabels}
\alias{processLSvarLabels}
\title{
A function to conveniently process LimeSurvey labels
}
\description{
This function is meant to quickly parse the variable labels set by LimeSurvey. It works particularly well with dual anchor array questions, where the left and right anchors as well as the subquestions are extracted automatically.
}
\usage{
processLSvarLabels(dat,
                   varnameRegExPairs = NULL,
                   labelExtractionRegExPair = c("\\\\[(.*)\\\\].*", "\\\\1"),
                   lengthToWrap = 50,
                   lengthToWrapAnchors = 20,
                   leftAnchorRegExPairs = list(c(".*[[:graph:]]([A-Z][a-z][^|]*)\\\\|(.+)",
                                                 "\\\\1")),
                   rightAnchorRegExPairs = list(c(".*[[:graph:]]([A-Z][a-z][^|]*)\\\\|(.+)",
                                                  "\\\\2")))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
  The dataframe as produced by \code{\link{importLimeSurveyData}}.
}
  \item{varnameRegExPairs}{
  Pairs of regular expressions to replace in the variable names. This is
  useful when some pattern can be applied to the variable names to, for
  example, add underscores te denote different parts of the variable name.
  This has to be a list of character vectors that each have length 2.
}
  \item{labelExtractionRegExPair}{
  The regular expression pair used to extract the labels.
}
  \item{lengthToWrap}{
  At how many characters to wrap the subquestions.
}
  \item{lengthToWrapAnchors}{
  At how many characters to wrap the anchors.
}
  \item{leftAnchorRegExPairs}{
  The regular expression pairs to use to extract the left anchors.
}
  \item{rightAnchorRegExPairs}{
  The regular expression pairs to use to extract the right anchors.
}
}
\details{
This function processes LimeSurvey variable labels and applies regular expressions
to automatically extract subquestions and left and right anchors.
}
\value{
A dataframe that can conveniently be used with \code{\link{detStructAddVarLabels}}.
}
\references{
(Forthcoming)
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{determinantStructure}}, \code{\link{determinantVar}},
\code{\link{subdeterminants}}, \code{\link{subdeterminantProducts}},
\code{\link{detStructAddVarLabels}}, \code{\link{detStructAddVarNames}},
\code{\link{detStructComputeProducts}}, \code{\link{detStructComputeScales}},
\code{\link{detStructCIBER}}
}
\examples{
### No examples provided yet; this would require data to be included,
### and that's not available yet.
}
\keyword{ utilities }
