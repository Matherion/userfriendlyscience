diamondPlot <- function(data,
                        ciCols=1:3,
                        colorCol=NULL,
                        otherAxisCol=NULL,
                        yValues=NULL, yLabels=NULL, ylab = NULL,
                        autoSize=NULL, fixedSize=.15,
                        xlab='Effect Size Estimate',
                        theme=theme_bw(), color='black',
                        returnLayerOnly = FALSE,
                        outputFile = NULL,
                        outputWidth = 10,
                        outputHeight = 10,
                        ggsaveParams = list(units='cm',
                                            dpi=300,
                                            type="cairo"),
                        ...) {

  ### In case we want to check for a complete dataframe
  # if (sum(complete.cases(data[, c(ciCols, colorCol, otherAxisCol)])) < nrow(data)) {
  #   warning("The dataframe passed in argument 'data' contained rows with missing values! I am removing these rows.");
  #   data <- data[complete.cases(data[, c(ciCols, colorCol, otherAxisCol)]), ];
  # }

  if (!is.null(yValues)) {
    ### Check whether yValues specifies a column in 'data' or whether it's a vector
    if (length(yValues) == 1) {
      ### Probably index in dataframe
      if (is.character(yValues) && (yValues %in% names(data))) {
        yValues <- data[, yValues];
      } else if (yValues <= ncol(data)) {
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

  ### Check whether yLabels specifies a column in 'data' or whether it's a vector
  if (!is.null(yLabels)) {
    if ((nrow(data) == 1) && (length(yLabels) == 1) && (is.character(yLabels))) {
      ### Don't do anything
    } else if (length(yLabels) == 1 && (nrow(data) > 1)) {
      ### Probably index in dataframe
      if (is.character(yLabels) && (yLabels %in% names(data))) {
        yLabels <- data[, yLabels];
      } else if (yLabels <= ncol(data)) {
        ### Consider it an index
        yLabels <- data[, yLabels];
      }
      ### Otherwise, we don't consider it an index (but as a vector of length one),
      ### so we keep it as is, just like when it /is/ a vector (of length > one)
    }
  } else if (class(yValues) == 'character') {
    yLabels <- yValues;
  } else if (!is.null(rownames(data))) {
    yLabels <- rownames(data);
  } else {
    yLabels <- yValues;
  }

  data$diamondPlotYLabelColumn <- diamondPlotYLabelColumn <- yLabels;

  if (length(colorCol) > 1) {
    if (length(colorCol) != nrow(data)) {
      stop("When specifying a vector as colorCol, this has ",
           "to be the same length as the dataframe!");
    }
    data$colorCol <- colorCol;
    colorCol <- 'colorCol';
  }

  if (is.null(otherAxisCol)) {
    data$otherAxisCol <- as.numeric(factor(yValues));
    otherAxisCol <- 'otherAxisCol';
  }

  diamondLayer <- ggDiamondLayer(data, ciCols = ciCols,
                                 colorCol = colorCol,
                                 otherAxisCol = otherAxisCol,
                                 autoSize=autoSize,
                                 fixedSize = fixedSize,
                                 color=color, ...);

  plot <- ggplot() +
    diamondLayer +
    scale_y_continuous(breaks=data$otherAxisCol, minor_breaks=NULL,
                       labels=diamondPlotYLabelColumn) +
    theme + ylab(ylab) + xlab(xlab) +
    theme(panel.grid.minor.y=element_blank());

  if (!is.null(outputFile)) {
    ggsaveParameters <- c(list(filename = outputFile,
                               plot = plot,
                               width = outputWidth,
                               height = outputHeight),
                          ggsaveParams);
    do.call(ggsave, ggsaveParameters);
  }

  if (returnLayerOnly) {
    return(diamondLayer);
  } else {
    return(plot);
  }

}
