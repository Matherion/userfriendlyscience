asymmetricalScatterMatrix <- function(dat, x, y,
                                      scaleLimits = NULL,
                                      powerHist=TRUE,
                                      theme=dlvTheme(),
                                      txtHeight = 1,
                                      histHeight = 5,
                                      scatterWidth = 10,
                                      scatterHeight = 10,
                                      unit = 'cm',
                                      dpi=300,
                                      ...) {
  
  ### Generate object with 3 sub-objects to store input,
  ### intermediate results, and output
  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());
  
  ### Extract dataframe and select only complete cases
  res$intermediate$dat <-
    dat <-
    na.omit(dat[, c(x, y)]);

  ### Convert all variables to numeric vectors, if they weren't already
  res$intermediate$dat <-
    dat <-
    massConvertToNumeric(res$intermediate$dat);
  
  res$intermediate$plots <- list();
  res$intermediate$plotList <- list();
  for (currentRowVar in -1:length(y)) {
    res$intermediate$plots[[currentRowVar+2]] <- list();
    for (currentColVar in -1:length(x)) {
      
      if ((currentRowVar < 1) && (currentColVar < 1)) {
        ### Top-left corner, display nothing
        res$intermediate$plots[[currentRowVar+2]][[currentColVar+2]] <-
          grid.rect(gp=gpar(col="white"));
        
      } else if (currentRowVar == -1) {
        ### In the first row: show column variable name
        res$intermediate$plots[[currentRowVar+2]][[currentColVar+2]] <-
          textGrob(x[currentColVar]);
        
      } else if (currentColVar == -1) {
        ### In the first column: show row variable name
        res$intermediate$plots[[currentRowVar+2]][[currentColVar+2]] <-
          textGrob(y[currentRowVar], rot=90);
        
      } else if (currentRowVar == 0) {
        ### In the second row: show column variable histogram
        res$intermediate$plots[[currentRowVar+2]][[currentColVar+2]] <-
          powerHist(dat[[x[[currentColVar]]]], xLabel=FALSE,
                    yLabel=FALSE, distributionLineSize=.5,
                    normalLineSize = .5)$plot +
          theme(axis.title=element_blank(),
                plot.margin=unit(rep(1, 4), "mm"));
        
      } else if (currentColVar == 0) {
        
        ### In the second column: show row variable histogram
        res$intermediate$plots[[currentRowVar+2]][[currentColVar+2]] <-
          powerHist(dat[[y[[currentRowVar]]]], xLabel=FALSE,
                    yLabel=FALSE, distributionLineSize=.5,
                    normalLineSize = .5)$plot +
          scale_y_reverse() + coord_flip() +
          theme(axis.title=element_blank(),
                plot.margin=unit(rep(1, 4), "mm"));
        
      } else {
        ### We're beyond the second row or column; show scatterplot
        
        ggplotDf <- data.frame(x = dat[, x[[currentColVar]] ],
                               y = dat[, y[[currentRowVar]] ]);
#         
#         corLabel <- data.frame(x = min(dat[, x[[currentColVar]] ]),
#                                y = min(dat[, y[[currentRowVar]] ]),
#                                label = noZero(round(cor(dat[[x[[currentColVar]]]],
#                                                         dat[[y[[currentRowVar]]]]), 3)));
        
        ### Create the plot; note that we start with adding the scatterplot
        ### to get the dimensions, then we add the text, and then the scatterplot
        ### again so it overlays the text.
        
        jitteredPointsLayer <- geom_point(aes(x=x, y=y), position='jitter');

        res$intermediate$plots[[currentRowVar+2]][[currentColVar+2]] <-
          #ggplot(dat, aes_string(x=x[[currentColVar]], y=y[[currentRowVar]])) +
          ggplot(data = ggplotDf, aes(x=x, y=y)) +
          #jitteredPointsLayer +
          geom_text(x = min(dat[, x[[currentColVar]] ]),
                    y = min(dat[, y[[currentRowVar]] ]),
                    label = noZero(round(cor(dat[[x[[currentColVar]]]],
                                             dat[[y[[currentRowVar]]]]), 3)),
                    size=20, color="#cadded", vjust=0, hjust=0) +
          geom_text(x = min(dat[, x[[currentColVar]] ]),
                    y = max(dat[, y[[currentRowVar]] ]),
                    label = noZero(round(cor(dat[[x[[currentColVar]]]],
                                             dat[[y[[currentRowVar]]]]), 3)),
                    size=20, color="#cadded", vjust=1, hjust=0) +
          geom_text(x = max(dat[, x[[currentColVar]] ]),
                    y = min(dat[, y[[currentRowVar]] ]),
                    label = noZero(round(cor(dat[[x[[currentColVar]]]],
                                             dat[[y[[currentRowVar]]]]), 3)),
                    size=20, color="#cadded", vjust=0, hjust=1) +
          geom_text(x = max(dat[, x[[currentColVar]] ]),
                    y = max(dat[, y[[currentRowVar]] ]),
                    label = noZero(round(cor(dat[[x[[currentColVar]]]],
                                             dat[[y[[currentRowVar]]]]), 3)),
                    size=20, color="#cadded", vjust=1, hjust=1) +
          jitteredPointsLayer +
          theme_bw() +
          theme(axis.title=element_blank(),
                plot.margin=unit(rep(1, 4), "mm"));
        
#         print(res$intermediate$plots[[currentRowVar+2]][[currentColVar+2]]);

        ### Add text - has to be separate, otherwise it changes the axes
#         res$intermediate$plots[[currentRowVar+2]][[currentColVar+2]] <-
#           res$intermediate$plots[[currentRowVar+2]][[currentColVar+2]] +
#           geom_text(x = min(dat[, x[[currentColVar]] ]),
#                     y = min(dat[, y[[currentRowVar]] ]),
#                     label = noZero(round(cor(dat[[x[[currentColVar]]]],
#                                              dat[[y[[currentRowVar]]]]), 3)),
#                     size=20, color="#cadded", vjust=0, hjust=0);
#           geom_text(data = corLabel,
#                     mapping = aes(x=x, y=y, label=label),
#                     size=20, color="#cadded", vjust=0, hjust=0) +
          
      }
      ### Now reorganise into one long list, from top-left to bottom-right
      res$intermediate$plotList[[length(res$intermediate$plotList) + 1]] <-
        res$intermediate$plots[[currentRowVar+2]][[currentColVar+2]];
    }
  }
  
  ### Create the final plot
  res$output$scatterMatrix <-
    do.call(arrangeGrob, c(res$intermediate$plotList,
                           list(ncol=length(x) + 2,
                                widths=unit(c(txtHeight, histHeight, rep.int(scatterWidth, length(x))), unit),
                                heights=unit(c(txtHeight, histHeight, rep.int(scatterHeight, length(y))), unit))));

  ### Store the size of the plot
  res$output$plotSize <- list(width = txtHeight + histHeight + scatterWidth * length(x),
                              height = txtHeight + histHeight + scatterHeight * length(y),
                              unit=unit,
                              res=dpi);
  
  ### Set class and return result
  class(res) <- "asymmetricalScatterMatrix";
  return(res);
  
}

print.asymmetricalScatterMatrix <- function(x, ...) {
  grid.draw(x$output$scatterMatrix, ...);
}
