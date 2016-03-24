gg_ciDiamondsLayer <- function(data,
                               ciCols=1:3, otherAxisCol=4,
                               autoSize=NULL, fixedSize=.25,
                               ...) {
  return(apply(data, 1, function(x, aSize=autoSize,
                                 fSize = fixedSize) {
    return(geom_polygon(CItoDiamondCoordinates(unlist(x[ciCols]),
                                               otherAxisValue=x[otherAxisCol],
                                               autoSize = aSize,
                                               fixedSize = fSize),
                        mapping=aes(x=x, y=y), ...));
  }));
}
