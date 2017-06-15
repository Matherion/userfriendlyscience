\name{setFigCapNumbering}
\alias{setFigCapNumbering}
\alias{setTabCapNumbering}
\title{
Automatic caption numbering knitr hooks for figures and tables
}
\description{
These function implement ideas by Max Gordon and DeanK (see Details) to add \code{\link{knitr}} hooks to automate the numbering of figures and tables when generating R Markdown documents.
}
\usage{
setFigCapNumbering(captionName = "fig.cap",
                   figure_counter_str = "Figure \%s: ",
                   figureClass = "", imgClass = "",
                   figureInlineStyle = c("display:block"),
                   imgInlineStyle = NULL,
                   optionName = paste0("setCaptionNumbering_", captionName),
                   resetCounterTo = 1)
setTabCapNumbering(table_counter_str = ":Table \%s: ",
                   resetCounterTo = 1)
}
\arguments{
  \item{captionName}{
  The name of the caption, used in the \code{\link{knitr}} chunk options to provide the caption text.
}
  \item{figure_counter_str, table_counter_str}{
  The string in which to add the number of the figure or table. The text '\%s' will be replaced by the number.
}
  \item{figureClass}{
  Optionally, a css class to pass to the <fig> HTML element that surrounds the <img>.
}
  \item{imgClass}{
  Optionall, a css class to pass to the <img> HTML element.
}
  \item{figureInlineStyle}{
  Any css style to pass to the figure element directly ('inline').
}
  \item{imgInlineStyle}{
  Any css style to pass to the image element directly ('inline').
}
  \item{optionName}{
  The name of the option to use to retrieve and set the counter. This can be used, for example, to have multiple caption types use the same counter.
}
  \item{resetCounterTo}{
  If not \code{NULL} and numeric, the counter will start at this number.
}
}
\details{
The figure caption function is basically the one designed by Max Gordon (see \url{http://gforge.se/2014/01/fast-track-publishing-using-knitr-part-iii/}.

The table caption function is an implementation of the ideas of DeanK (see \url{http://stackoverflow.com/questions/15258233/using-table-caption-on-r-markdown-file-using-knitr-to-use-in-pandoc-to-convert-t}) combined with Max Gordon's function.
}
\value{
Nothing is returned; the correct hooks are configured for \code{\link{knitr}}.
}
\author{
Max Gordon (setFigCapNumbering) and DeanK (setTabCapNumbering); implemented by Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{knitr}}
}
\examples{
\dontrun{
  setFigCapNumbering("This is figure number \%s, with caption text: ");
}
}
\keyword{ utils }
