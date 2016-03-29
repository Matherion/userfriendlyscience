diamondPlot <- function(data,
                          ciCols=1:3, otherAxisCol=NULL,
                          yValues=NULL, yLabels=NULL, ylab = NULL,
                          autoSize=NULL, fixedSize=.15,
                          xlab='Effect Size Estimate',
                          theme=dlvTheme(), color='black', ...) {
  
  if (!is.null(yValues)) {
    ### Check whether yValues specifies a column in 'data' or whether it's a vector
    if (length(yValues) == 1) {
      ### Probably index in dataframe
      if (is.character(yValues) && (yValues %in% names(data))) {
        yValues <- data[, yValues];
      } else if (yValues < ncol(data)) {
        ### Consider it an index
        yValues <- data[, yValues]
      }
      ### Otherwise, we don't consider it an index (but as a vector of length one),
      ### so we keep it as is, just like when it /is/ a vector (of length > one)
    }
  }
  
  if (is.null(yValues)) {
    yValues <- 1:nrow(data);
  }

  if (!is.null(yLabels)) {
    ### Check whether yLabels specifies a column in 'data' or whether it's a vector
    if (length(yLabels) == 1) {
      ### Probably index in dataframe
      if (is.character(yLabels) && (yLabels %in% names(data))) {
        yLabels <- data[, yLabels];
      } else if (yLabels < ncol(data)) {
        ### Consider it an index
        yLabels <- data[, yLabels]
      }
      ### Otherwise, we don't consider it an index (but as a vector of length one),
      ### so we keep it as is, just like when it /is/ a vector (of length > one)
    }
  } else if (!is.null(rownames(data))) {
    yLabels <- rownames(data);
  } else {
    yLabels <- yValues;
  }

  if (is.null(otherAxisCol)) {
    data$otherAxisCol <- yValues;
    otherAxisCol <- 'otherAxisCol';
  }

  return(ggplot() +
           gg_diamondLayer(data, ciCols = ciCols,
                           otherAxisCol = otherAxisCol,
                           autoSize=autoSize,
                           fixedSize = fixedSize,
                           color=color,...) +
           scale_y_continuous(breaks=yValues, labels=yLabels) +
           theme + ylab(ylab) + xlab(xlab));
}
