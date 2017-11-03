\name{knitFig}
\alias{knitFig}
\title{
Easily knit a custom figure fragment
}
\description{
THis function was written to make it easy to knit figures with different, or dynamically generated, widths and heights (and captions) in the same chunk when working with R Markdown.
}
\usage{
knitFig(plotToDraw,
        template = getOption("ufs.knitFig.template", NULL),
        figWidth = getOption("ufs.knitFig.figWidth", 16/2.54),
        figHeight = getOption("ufs.knitFig.figHeight", 16/2.54),
        figCaption = "A plot.",
        chunkName = NULL, ...)
}
\arguments{
  \item{plotToDraw}{
  The plot to draw, e.g. a \code{\link{ggplot}} plot.
}
  \item{template}{
  A character value with the \code{\link{knit_expand}} template to use.
}
  \item{figWidth}{
  The width to set for the figure (in inches).
}
  \item{figHeight}{
  The height to set for the figure (in inches).
}
  \item{figCaption}{
  The caption to set for the figure.
}
  \item{chunkName}{
  Optionally, the name for the chunk. To avoid problems because multiple chunks have the name "\code{unnamed-chunk-1}", if no chunk name is provided, \code{\link{digest}} is used to generate an MD5-hash from \code{\link{Sys.time}}.
}
  \item{\dots}{
  Any additional arguments are passed on to \code{\link{knit_expand}}.
}
}
\value{
This function returns nothing, but uses \code{\link{knit_expand}} and \code{\link{knit}} to \code{\link{cat}} the result.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{knit_expand}} and \code{\link{knit}}
}
\examples{
\dontrun{
knitFig(ggProportionPlot(mtcars$cyl))
}
}
\keyword{ utilities }
