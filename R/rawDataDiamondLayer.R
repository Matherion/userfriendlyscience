rawDataDiamondLayer <- function(dat, items = NULL, itemOrder = 1:length(items),
                                dataAlpha = .1,
                                dataColor = "#444444",
                                jitterWidth = .5,
                                jitterHeight = .4,
                                ...) {

  rawData <- na.omit(data.frame(value = unlist(dat[, items[itemOrder]]),
                                labels = rep(1:length(items),
                                             each=nrow(dat))));
  
  rawDataLayer <- geom_jitter(data=rawData,
                              mapping=aes_string(x='value', y='labels'),
                              size = 2.5,
                              color = dataColor,
                              alpha = dataAlpha,
                              stroke = 0,
                              width=jitterWidth,
                              height=jitterHeight,
                              ...);  
  
  return(rawDataLayer);
  
}
