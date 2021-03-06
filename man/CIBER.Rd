\name{CIBER}
\alias{CIBER}
\alias{detStructCIBER}
\alias{detStructCIBER}
\title{Confidence Interval-Based Estimation of Relevance (CIBER)}
\usage{
CIBER(data, determinants, targets, conf.level = list(means = 0.9999,
  associations = 0.95), subQuestions = NULL, leftAnchors = rep("Lo",
  length(determinants)), rightAnchors = rep("Hi", length(determinants)),
  orderBy = NULL, decreasing = NULL, numberSubQuestions = FALSE,
  generateColors = list(means = c("red", "blue", "green"), associations =
  c("red", "grey", "green")),
  strokeColors = viridis::viridis(length(targets)),
  titlePrefix = "Means and associations with", titleVarLabels = NULL,
  titleSuffix = "", fullColorRange = NULL, associationsAlpha = 0.5,
  returnPlotOnly = TRUE, drawPlot = TRUE, baseSize = 0.8, dotSize = 2.5
  * baseSize, baseFontSize = 10 * baseSize,
  theme = ggplot2::theme_bw(base_size = baseFontSize), xbreaks = NULL, ...)

detStructCIBER(determinantStructure, data, conf.level = list(means = 0.9999,
  associations = 0.95), subQuestions = NULL, leftAnchors = rep("Lo",
  length(determinants)), rightAnchors = rep("Hi", length(determinants)),
  orderBy = 1, decreasing = NULL, generateColors = list(means = c("red",
  "blue", "green"), associations = c("red", "grey", "green")),
  strokeColors = NULL, titlePrefix = "Means and associations with",
  titleVarLabels = NULL, titleSuffix = "", fullColorRange = NULL,
  associationsAlpha = 0.5, baseSize = 0.8, dotSize = 2.5 * baseSize,
  baseFontSize = 10 * baseSize, theme = ggplot2::theme_bw(base_size =
  baseFontSize), ...)
}
\arguments{
\item{data}{The dataframe containing the variables.}

\item{determinants}{The 'determinants': the predictors (or 'covariates') of
the target variables(s) (or 'criteria').}

\item{targets}{The 'targets' or 'criteria' variables: the variables
predicted by the determinants.}

\item{conf.level}{The confidence levels for the confidence intervals: has to
be a named list with two elements: \code{means} and \code{associations},
specifying the desired confidence levels for the means and associations,
respectively. The confidence level for the associations is also used for the
intervals for the proportions of explained variance.}

\item{subQuestions}{The subquestions used to measure each determinants. This
can also be used to provide pretty names for the variables if the
determinants were not measured by one question each. Must have the same
length as \code{determinants}.}

\item{leftAnchors}{The anchors to display on the left side of the left hand
panel. If the determinants were measured with one variable each, this can be
used to show the anchors that were used for the respective scales. Must have
the same length as \code{determinants}.}

\item{rightAnchors}{The anchors to display on the left side of the left hand
panel. If the determinants were measured with one variable each, this can be
used to show the anchors that were used for the respective scales. Must have
the same length as \code{determinants}.}

\item{orderBy}{Whether to sort the determinants. Set to \code{NULL} to not
sort at all; specify the name or index of one of the \code{target}s to sort
by the point estimates of the associations with that target variable. Use
\code{decreasing} to determine whether to sort in ascending or descending
order. For convenience, if \code{orderBy} is not \code{NULL}, but
\code{decreasing} is, the determinants are sorted in descending (decreasing)
order.}

\item{decreasing}{Whether to sort the determinants. Specify \code{NULL} to
not sort at all, \code{TRUE} to sort in descending order, and \code{FALSE}
to sort in ascending order. If \code{decreasing} is nor \code{NULL}, but
\code{orderBy} is \code{NULL}, the determinants are sorted by their means.
For convenience, if \code{orderBy} is not \code{NULL}, but \code{decreasing}
is, the determinants are sorted in descending (decreasing) order.}

\item{numberSubQuestions}{Whether or not to number the subquestions. If they
are numbered, they are numbered from the top to the bottom.}

\item{generateColors}{The colors to use to generate the gradients for
coloring the diamonds representing the confidence intervals. Has to be a
named list with two elements: \code{means} and \code{associations},
specifying the desired colors for the means and associations, respectively.}

\item{strokeColors}{The palette to use to color the stroke of the confidence
intervals for the associations between the determinants and the targets.
Successive colors from this palette are used for the targets.}

\item{titlePrefix}{Text to add before the list of target names and the
proportions of explained variance for each target. This plot title also
serves as legend to indicate which target 'gets' which each color.}

\item{titleVarLabels}{Optionally, variable labels to use in the plot title.
Has to be the exact same length as \code{targets}.}

\item{titleSuffix}{Text to add after the list of target names and the
proportions of explained variance for each target.}

\item{fullColorRange}{If colors are specified, this can be used to specify
which values, for the determinant confidence intervals in the left hand
panel, are the minimum and maximum. This is useful if those scores are not
actually in the data (e.g. for extremely skewed distributions). If
\code{NULL}, the range of all individual scores on the determinants is used.
For the associations, \code{c(-1, 1)} is always used as
\code{fullColorRange}.}

\item{associationsAlpha}{The alpha level (transparency) of the confidence
interval diamonds in the right hand plot. Value between 0 and 1, where 0
signifies complete transparency (i.e. invisibility) and 1 signifies complete
'opaqueness'.}

\item{returnPlotOnly}{Whether to return the entire object that is generated
(including all intermediate objects) or only the plot.}

\item{drawPlot}{Whether the draw the plot, or only return it.}

\item{baseSize}{This can be used to efficiently change the size of most plot
elements.}

\item{dotSize}{This is the size of the points used to show the individual
data points in the left hand plot.}

\item{baseFontSize}{This can be used to set the font size separately from
the \code{baseSize}.}

\item{theme}{This is the theme that is used for the plots.}

\item{xbreaks}{Which breaks to use on the X axis (can be useful to override
\code{\link{ggplot2}}'s defaults).}

\item{\dots}{These arguments are passed on to
\code{\link{biAxisDiamondPlot}} (for the left panel) and
\code{\link{diamondPlot}} (for the right panel).  Note that all argument are
passed to both those functions.}

\item{determinantStructure}{When using \code{detStructCIBER}, the
determinant structure as generated by \code{\link{determinantStructure}} is
included here. \code{determinants}, \code{targets}, \code{subQuestions},
\code{leftAnchors}, and \code{rightAnchors} are then read from the
\code{\link{determinantStructure}} object. In other words: once a
\code{\link{determinantStructure}} has been generated, only \code{dat} and
\code{\link{determinantStructure}} have to be provided as argument to
generate a CIBER diamond plot.}
}
\value{
Depending on the value of \code{returnPlotOnly}, either the plot
only (a \code{\link{gtable}} object) or an object containing most objects
created along the way (in which case the plot is stored in
\code{$output$plot}).

The plot has \code{width} and \code{height} attributes which can be used
when saving the plot.
}
\description{
This function generates a high-level plot consisting of several diamond
plots. This function is useful for estimating the relative relevance of a
set of determinants of, for example, behavior. The plot in the left hand
panel shows each determinant's distribution with a diamond representing the
confidence interval. The right hand plot shows the determinants'
associations to one or more 'target' variables, such as behavior or
determinants of behavior.
}
\details{
Details are explained in Crutzen & Peters (2017).
}
\examples{
\dontrun{
CIBER(data=mtcars,
      determinants=c('drat', 'wt', 'am',
                     'gear', 'vs', 'carb'),
      targets=c('mpg', 'cyl'));
}
}
\references{
Crutzen, R., Peters, G.-J. Y., & Noijen, J. (2017). How to
Select Relevant Social-Cognitive Determinants and Use them in the
Development of Behaviour Change Interventions? Confidence Interval-Based
Estimation of Relevance. http://dx.doi.org/
}
\seealso{
\code{\link{determinantStructure}}
}
\keyword{hplot}
