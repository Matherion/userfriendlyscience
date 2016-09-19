factorLoadingDiamondCIplot <- function(fa, autoSize=NULL, fixedSize=.25,
                                       xlab='Factor Loading',
                                       geomAlpha=.3,
                                       colors = c('red', 'green')) {
  
  ### Combine both confidence intervals and factor loadings, using
  ### the code from the 'psych:::print.psych.fa.ci' function 
  lc <- data.frame(unclass(fa$loadings), fa$ci$ci);
  ### Create list for CIs per factor
  CIs <- list();
  for (i in 1:fa$factors) {
    CIs[[i]] <- lc[, c(i + fa$factors, i, i + fa$factors * 2)];
    CIs[[i]][, 4] <- 1:nrow(CIs[[i]]);
    names(CIs[[i]]) <- c('lo', 'est', 'hi', 'variable');
  }

  ### Create empty
  res <- ggplot();

  for (currentFactor in 1:length(CIs)) {
    res <- res + apply(CIs[[currentFactor]], 1, function(x, aSize=autoSize,
                                         fSize = fixedSize, alpha=geomAlpha,
                                         color = colors[currentFactor]) {
      return(geom_polygon(CItoDiamondCoordinates(unlist(x[1:3]),
                                                 otherAxisValue=x[4],
                                                 autoSize = aSize,
                                                 fixedSize = fSize),
                          mapping=aes(x=x, y=y), alpha=alpha,
                          color=color, fill=color));
    });
  }

  res <- res + scale_y_continuous(breaks=1:nrow(unclass(fa$loadings)),
                          labels=rownames(unclass(fa$loadings))) +
    ylab(NULL) + xlab(xlab) + theme_bw() + geom_vline(xintercept=0);

  return(res);
}
