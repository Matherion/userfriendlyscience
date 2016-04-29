### This function actually makes the scales
makeScales <- function(dat, scales) {
  for (currentScale in 1:length(scales)) {
    if (length(unlist(scales[currentScale])) > 1) {
      dat[[names(scales[currentScale])]] <-
        rowMeans(dat[, unlist(scales[currentScale])], na.rm=TRUE);
    }
    else if (length(unlist(scales[currentScale])) == 1) {
      dat[[names(scales[currentScale])]] <- dat[[unlist(scales[currentScale])]];
    }
  }
  return(dat);
}
