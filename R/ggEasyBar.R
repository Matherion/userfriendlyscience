ggEasyBar <- function(data, items = NULL,
                      labels = NULL, sortByMean = TRUE,
                      xlab = NULL, ylab = NULL,
                      scale_fill_function = scale_fill_viridis(discrete = TRUE,
                                                               guide = guide_legend(title = NULL,
                                                                                    nrow=1)),
                      fontColor = "white",
                      fontSize = 2,
                      labelMinPercentage = 1,
                      showInLegend = "both") {
  
  if (is.null(items)) {
    items <- names(data);
  }
  
  if (!all(items %in% names(data))) {
    stop("You specified items that do not exist in the data you provided (specifically, ",
         vecTxtQ(items[!items %in% names(data)]), ").");
  }
  
  if (sortByMean && length(items) > 1) {
    tmpVarOrder <- order(colMeans(data[, items],
                                  na.rm=TRUE),
                         decreasing=TRUE);
  } else {
    tmpVarOrder <- 1:length(items);
  }
  
  if (is.null(labels)) {
    labels <- items;
  }
  
  ### Get frequencies and store them
  tmpDf <- lapply(data[, items, drop=FALSE], function(x) return(cbind(table(x), table(x) / sum(table(x)))));
  tmpDf <- lapply(names(tmpDf), function(x) return(data.frame(var = rep(x, nrow(tmpDf[[x]])),
                                                              val = rownames(tmpDf[[x]]),
                                                              abs = tmpDf[[x]][, 1],
                                                              rel = 100 * tmpDf[[x]][, 2])));

  tmpDf <- do.call(rbind, tmpDf);
  ### Convert row names to numeric if need be
  if (!is.numeric(tmpDf$val)) {
    if (all(grepl('\\d+', tmpDf$val))) {
      if (is.factor(tmpDf$val)) {
        tmpDf$val <- as.numeric(levels(tmpDf$val))[tmpDf$val];
      } else {
        tmpDf$val <- as.numeric(tmpDf$val);
      }
    }
  }
  
  if (is.numeric(tmpDf$val) || (all(grepl('\\d+', tmpDf$val)))) {
    tmpDf$val <- factor(tmpDf$val,
                        levels = sort(as.numeric(unique(tmpDf$val))),
                        labels = sort(as.numeric(unique(tmpDf$val))),
                        ordered=TRUE);
  }
  
  tmpDf$var <- factor(tmpDf$var,
                      levels=items[tmpVarOrder],
                      labels=labels[tmpVarOrder],
                      ordered=TRUE);

  if (showInLegend == "both") {
    tmpDf$val <- paste0(tmpDf$val, " (", tmpDf$abs, "; ", round(tmpDf$rel), "%)");
  } else if (showInLegend == "perc") {
    tmpDf$val <- paste0(tmpDf$val, " (", round(tmpDf$rel), "%)");
  } else if (showInLegend == "freq") {
    tmpDf$val <- paste0(tmpDf$val, " (", tmpDf$abs, ")");
  }

  tmpDf$label <- ifelse(round(tmpDf$rel) >= labelMinPercentage,
                        paste0(tmpDf$abs,
                               "\n(",
                               round(tmpDf$rel),
                               "%)"),
                        "");

  ### Actual plot
  res <- ggplot(data = tmpDf,
                mapping = aes_string(x = 'var',
                                     y = 'rel',
                                     fill = 'val',
                                     label = 'label')) +
    geom_bar(na.rm=TRUE, stat = 'identity',
             position = position_stack(reverse = TRUE)) +
    theme_minimal() +
    coord_flip() +
    scale_fill_function +
    geom_text(color=fontColor, size = fontSize,
              position = position_stack(reverse=TRUE, vjust = 0.5)) +
    labs(x=xlab, y=ylab) +
    theme(legend.position="bottom");
  
  return(res);
}
