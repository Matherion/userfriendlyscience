gg_diamondLayer <- function(data,
                               ciCols=1:3, otherAxisCol=1:nrow(data),
                               autoSize=NULL, fixedSize=.15,
                               ...) {
  if (length(otherAxisCol) > 1) {
    data[, 'otherAxisValues'] <- otherAxisCol;
    otherAxisCol <- otherAxisValues;
  }
  return(apply(data, 1, function(x, aSize=autoSize,
                                 fSize = fixedSize) {
    return(geom_polygon(diamondCoordinates(unlist(x[ciCols]),
                                               otherAxisValue=x[[otherAxisCol]],
                                               autoSize = aSize,
                                               fixedSize = fSize),
                        mapping=aes(x=x, y=y), ...));
  }));
}
