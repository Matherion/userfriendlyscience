factorLoadingDiamondCIplot <- function(fa,
                                       xlab='Factor Loading',
                                       colors = viridis_pal()(max(2, fa$factors)),
                                       labels=NULL,
                                       theme=theme_bw(),
                                       ...) {
  
  ### Create list for CIs per factor
  CIs <- faConfInt(fa);

  dotsList <- as.list(substitute(list(...)));
  
  if ('alpha' %in% names(dotsList)) {
    alpha <- dotsList$alpha;
  } else {
    alpha <- 1;
  }
  
  ### Create empty
  res <- ggplot(data.frame(Factor=as.factor(1:length(CIs))),
                aes_string(x=-Inf, ymin=-Inf, ymax=-Inf,
                           color='Factor', fill='Factor')) +
    geom_ribbon() +
    geom_vline(xintercept=0) +
    scale_color_manual(values=colors) +
    scale_fill_manual(values=alpha(colors, alpha));
  
  for (currentFactor in 1:length(CIs)) {
    res <- res + ggDiamondLayer(CIs[[currentFactor]],
                                color = colors[currentFactor],
                                ...);
  }
  
  if (is.null(labels)) {
    labels <- rownames(unclass(fa$loadings));
  }
  
  res <- res +
    scale_y_continuous(breaks=1:nrow(unclass(fa$loadings)),
                       labels=labels) +
    ylab(NULL) + xlab(xlab) + theme;

  return(res);
}
