#' Easily knit a custom figure fragment
#' 
#' THis function was written to make it easy to knit figures with different, or
#' dynamically generated, widths and heights (and captions) in the same chunk
#' when working with R Markdown.
#' 
#' 
#' @param plotToDraw The plot to draw, e.g. a \code{\link{ggplot}} plot.
#' @param template A character value with the \code{\link{knit_expand}}
#' template to use.
#' @param figWidth The width to set for the figure (in inches).
#' @param figHeight The height to set for the figure (in inches).
#' @param figCaption The caption to set for the figure.
#' @param chunkName Optionally, the name for the chunk. To avoid problems
#' because multiple chunks have the name "\code{unnamed-chunk-1}", if no chunk
#' name is provided, \code{\link{digest}} is used to generate an MD5-hash from
#' \code{\link{Sys.time}}.
#' @param \dots Any additional arguments are passed on to
#' \code{\link{knit_expand}}.
#' @return This function returns nothing, but uses \code{\link{knit_expand}}
#' and \code{\link{knit}} to \code{\link{cat}} the result.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{knit_expand}} and \code{\link{knit}}
#' @keywords utilities
#' @examples
#' 
#' \dontrun{
#' knitFig(ggProportionPlot(mtcars$cyl))
#' }
#' 
#' @export knitFig
knitFig <- function(plotToDraw,
                    template = getOption("ufs.knitFig.template", NULL),
                    figWidth = getOption("ufs.knitFig.figWidth", 16 / 2.54),
                    figHeight = getOption("ufs.knitFig.figHeight", 16 / 2.54),
                    figCaption = "A plot.",
                    chunkName = NULL,
                    ...) {
  if (is.null(template)) {
    template <- "\n\n```{r {{chunkName}}, fig.height={{figHeight}}, fig.width={{figWidth}}, fig.cap='{{figCaption}}', echo=FALSE, cache=FALSE, message=FALSE, results='asis' }
  grid.newpage();
  grid.draw(tmpPlotStorage);
```\n\n";
  }
  assign('tmpPlotStorage', plotToDraw);
  if (is.null(chunkName)) {
    chunkName <- digest(Sys.time());
  }
  cat(knit(text = knit_expand(text = template,
                              figWidth = figWidth,
                              figHeight = figHeight,
                              figCaption = figCaption,
                              chunkName = chunkName,
                              ...),
           quiet = TRUE));
  }
