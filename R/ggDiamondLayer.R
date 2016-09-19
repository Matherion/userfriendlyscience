ggDiamondLayer <- function(data,
                            ciCols=1:3,
                            colourCol=NULL,
                            generateColours = NULL,
                            color="black",
                            otherAxisCol=1:nrow(data),
                            autoSize=NULL,
                            fixedSize=.15,
                            ...) {
  if (length(otherAxisCol) > 1) {
    data[, 'otherAxisValues'] <- otherAxisCol;
    otherAxisCol <- 'otherAxisValues';
  }
  
  if (!is.null(colourCol) && !is.null(generateColours)) {
    data[, ncol(data) + 1] <- colorRampPalette(generateColours)(nrow(data));
    colourCol <- ncol(data);
  }

  return(apply(data, 1, function(x,
                                 cCol=colourCol,
                                 aSize=autoSize,
                                 fSize = fixedSize) {
    tmpDf <- data.frame(diamondCoordinates(as.numeric(unlist(x[ciCols])),
                                           otherAxisValue=as.numeric(x[[otherAxisCol]]),
                                           autoSize = aSize,
                                           fixedSize = fSize));
    if (is.null(cCol)) {
      return(geom_polygon(tmpDf,
                          mapping=aes(x=x, y=y), color=color, ...));
    } else {
      return(geom_polygon(tmpDf,
                          mapping=aes(x=x, y=y),
                          fill=x[[cCol]],
                          color = x[[cCol]], ...));
    }
  }));
}
