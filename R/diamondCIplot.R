diamondCIplot <- function(data,
                          ciCols=1:3, otherAxisCol=4,
                          yValues, yLabels, ylab = NULL,
                          autoSize=NULL, fixedSize=.25,
                          xlab='Effect Size Estimate',
                          theme=dlvTheme(), color='black', ...) {
  return(ggplot() +
           gg_ciDiamondsLayer(data, ciCols = ciCols,
                              otherAxisCol = otherAxisCol,
                              autoSize=autoSize,
                              fixedSize = fixedSize,
                              color=color,...) +
           scale_y_continuous(breaks=yValues, labels=yLabels) +
           theme + ylab(ylab) + xlab(xlab));
}
