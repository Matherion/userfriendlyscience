scatterPlot <- function(x, y, pointsize=3,
                        theme = theme_bw(),
                        regrLine = FALSE,
                        regrCI = FALSE,
                        regrLineCol = "blue",
                        regrCIcol = regrLineCol,
                        regrCIalpha = .25,
                        width = 0,
                        height = 0,
                        position='identity',
                        ...) {
  xVarName <- extractVarName(deparse(substitute(x)));
  yVarName <- extractVarName(deparse(substitute(y)));
  dat <- data.frame(x, y);
  names(dat) <- c(xVarName, yVarName);
  plot <- ggplot(dat, aes_string(xVarName, yVarName)) +
    theme;
  if (regrLine && regrCI) {
    plot <- plot + geom_smooth(method='lm', color = regrLineCol,
                               fill = regrCIcol, alpha = regrCIalpha);
  } else if (regrLine) {
    plot <- plot + geom_smooth(method='lm', color = regrLineCol,
                               se=FALSE);
  }
  if ((tolower(position)=='jitter') && (width == 0) && (height == 0)) {
    plot <- plot + geom_jitter(na.rm=TRUE,
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
