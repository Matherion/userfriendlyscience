### This function actually makes the scales
makeScales <- function(dat, scales, append=TRUE) {
  resDat <- dat[, FALSE];
  for (currentScale in 1:length(scales)) {
    if (length(unlist(scales[currentScale])) > 1) {
      resDat[[names(scales[currentScale])]] <-
        rowMeans(dat[, unlist(scales[currentScale])], na.rm=TRUE);
      resDat[[names(scales[currentScale])]] <-
        ifelse(is.nan(resDat[[names(scales[currentScale])]]),
               NA,
               resDat[[names(scales[currentScale])]]);
      attributes(resDat[[names(scales[currentScale])]])$scale_item_names <-
        unname(unlist(scales[currentScale]));
    }
    else if (length(unlist(scales[currentScale])) == 1) {
      resDat[[names(scales[currentScale])]] <- dat[[unlist(scales[currentScale])]];
      attributes(resDat[[names(scales[currentScale])]])$scale_item_names <-
        unname(unlist(scales[currentScale]));
    }
  }
  if (append) {
    return(cbind(dat, resDat));
  } else {
    return(resDat);
  }
}
