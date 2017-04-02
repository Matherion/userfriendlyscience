ggDiamondLayer <- function(data,
                           ciCols=1:3,
                           colorCol=NULL,
                           generateColors = NULL,
                           fullColorRange = NULL,
                           color="black",
                           lineColor=NA,
                           otherAxisCol=1:nrow(data),
                           autoSize=NULL,
                           fixedSize=.15,
                           ...) {

  ### Set column with y axis values
  if (length(otherAxisCol) > 1) {
    data[, 'otherAxisValues'] <- otherAxisCol;
    otherAxisCol <- 'otherAxisValues';
  }

  ### If we need to generate colours, do so and set color column
  if (!is.null(generateColors)) {
    # data[, ncol(data) + 1] <- colorRampPalette(generateColours)(nrow(data));
    # colourCol <- ncol(data);

    if (is.null(fullColorRange)) {
      fullColorRange <- c(min(unlist(data[[ciCols[2]]])),
                          max(unlist(data[[ciCols[2]]])));
    }
    data[, ncol(data) + 1] <- scales::rescale(unlist(data[[ciCols[2]]]),
                                              to = c(0,1),
                                              from = fullColorRange);
    colorPositionCol <- ncol(data);
    colorPaletteFunction <- colorRamp(generateColors);

    data[!is.na(data[, colorPositionCol]), ncol(data) + 1] <-
      rgb(colorPaletteFunction(data[!is.na(data[, colorPositionCol]), colorPositionCol]) / 256);
    colorCol <- ncol(data);

  }

  return(apply(data, 1, function(x,
                                 cCol=colorCol,
                                 aSize=autoSize,
                                 fSize = fixedSize) {
    tmpDf <- data.frame(diamondCoordinates(as.numeric(unlist(x[ciCols])),
                                           otherAxisValue=as.numeric(x[[otherAxisCol]]),
                                           autoSize = aSize,
                                           fixedSize = fSize));
    if (is.null(cCol)) {
      return(geom_polygon(tmpDf,
                          mapping=aes(x=x, y=y),
                          fill=color,
                          color=ifelse(is.na(lineColor), color, lineColor), ...));
    } else {
      return(geom_polygon(tmpDf,
                          mapping=aes(x=x, y=y),
                          fill=x[[cCol]],
                          color = ifelse(is.na(lineColor), x[[cCol]], lineColor), ...));
    }
  }));
}
