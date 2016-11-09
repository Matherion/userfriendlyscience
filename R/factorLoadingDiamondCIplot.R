factorLoadingDiamondCIplot <- function(fa,
                                       xlab='Factor Loading',
                                       colors = brewer.pal(max(3, fa$factors), "Set1"),
                                       ...) {
  
  ### Combine both confidence intervals and factor loadings, using
  ### the code from the 'psych:::print.psych.fa.ci' function 
  lc <- data.frame(unclass(fa$loadings), fa$ci$ci);
  ### Create list for CIs per factor
  CIs <- faConfInt(fa);

  ### Create empty
  res <- ggplot();
  
  for (currentFactor in 1:length(CIs)) {
    
    res <- res + ggDiamondLayer(CIs[[currentFactor]],
                                color = colors[currentFactor],
                                ...);
  }
  
  res <- res + scale_y_continuous(breaks=1:nrow(unclass(fa$loadings)),
                          labels=rownames(unclass(fa$loadings))) +
    ylab(NULL) + xlab(xlab) + theme_bw() + geom_vline(xintercept=0);

  return(res);
}
