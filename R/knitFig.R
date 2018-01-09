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
