scatterPlot <- function(x, y, pointsize=3,
                        theme = theme_bw(),
                        regrLine = FALSE,
                        regrCI = FALSE,
                        regrLineCol = "blue",
                        regrCIcol = regrLineCol,
                        regrCIalpha = .25,
                        width = NULL,
                        height = NULL,
                        position='identity',
                        xVarName=NULL,
                        yVarName=NULL,
                        ...) {
  xVarName <- ifelse(is.null(xVarName),
                     extractVarName(deparse(substitute(x))),
                     xVarName);
  yVarName <- ifelse(is.null(yVarName),
                     extractVarName(deparse(substitute(y))),
                     yVarName);

  dat <- data.frame(x, y);
  names(dat) <- c(xVarName, yVarName);
  plot <- ggplot(dat, aes_string(xVarName, yVarName)) +
    theme;
  if (regrLine && regrCI) {
    plot <- plot + geom_smooth(method='lm', color = regrLineCol,
                               fill = regrCIcol, alpha = regrCIalpha,
                               na.rm=TRUE);
  } else if (regrLine) {
    plot <- plot + geom_smooth(method='lm', color = regrLineCol,
                               se=FALSE, na.rm=TRUE);
  }
  if ((tolower(position)=='identity') && (width == 0) && (height == 0)) {
    plot <- plot + geom_point(na.rm=TRUE,
                              size=pointsize,
                              ...);
  } else {
    plot <- plot + geom_jitter(na.rm=TRUE,
                               size=pointsize,
                               width = width,
                               height = height,
                               ...);
  }
  return(plot);
}
