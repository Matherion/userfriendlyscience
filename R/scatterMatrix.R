#' scatterMatrix
#' 
#' scatterMatrix produced a matrix with jittered scatterplots, histograms, and
#' correlation coefficients.
#' 
#' 
#' @param dat A dataframe containing the items in the scale. All variables in
#' this dataframe will be used if items is NULL.
#' @param items If not NULL, this should be a character vector with the names
#' of the variables in the dataframe that represent items in the scale.
#' @param plotSize Size of the final plot in millimeters.
#' @param sizeMultiplier Allows more flexible control over the size of the plot
#' elements
#' @param axisLabels Passed to ggpairs function to set axisLabels.
#' @param powerHist Whether to use the default ggpairs histogram on the
#' diagonal of the scattermatrix, or whether to use the powerHist version.
#' @param ...  Additional arguments are passed on to powerHist.
#' @return
#' 
#' An object with the input and several output variables. Most notably:
#' \item{output$scatterMatrix}{A scattermatrix with histograms on the diagonal
#' and correlation coefficients in the upper right half.}
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utilities univar
#' @examples
#' 
#' ### Note: the 'not run' is simply because running takes a lot of time,
#' ###       but these examples are all safe to run!
#' \dontrun{
#' 
#' ### Generate a datafile to use
#' exampleData <- data.frame(item1=rnorm(100));
#' exampleData$item2 <- exampleData$item1+rnorm(100);
#' exampleData$item3 <- exampleData$item1+rnorm(100);
#' exampleData$item4 <- exampleData$item2+rnorm(100);
#' exampleData$item5 <- exampleData$item2+rnorm(100);
#' 
#' ### Use all items
#' scatterMatrix(dat=exampleData);
#' }
#' 
#' @export scatterMatrix
scatterMatrix <- function(dat, items=NULL, plotSize=180, sizeMultiplier = 1,
                          axisLabels = "none", powerHist=TRUE, ...) {

  if (is.null(items)) {
    items <- names(dat);
  }
  
  ### Generate object with 3 sub-objects to store input,
  ### intermediate results, and output
  res <- list(input = list(dat=dat,
                           items=items,
                           plotSize=plotSize,
                           sizeMultiplier=sizeMultiplier,
                           axisLabels=axisLabels),
              intermediate = list(),
              output = list());
  
  ### Extract dataframe and select only complete cases
  res$intermediate$dat <- dat[complete.cases(dat[, items]), items];

  ### Convert all variables to numeric vectors, if they weren't already
  res$intermediate$dat <- data.frame(lapply(res$intermediate$dat, 'as.numeric'));
  
  ### The size of each panel in the scattermatrix depends
  ### on the number of items - therefore, we need to adjust
  ### the plot sizes to the number of items.
  res$intermediate$baseSize <- baseSize <-
    (sizeMultiplier * (plotSize / length(items))) / 100;
  
  res$intermediate$plotSettings <- plotSettings <-
    theme(axis.line = element_line(size = baseSize),
          panel.grid.major = element_line(size = baseSize/2),
          line = element_line(size = baseSize/2),
          axis.ticks = element_line (size=baseSize/2)
    );  
  
  ### Visual representation of bivariate correlations
  ### First generate a normal scattermatrix with histograms
  ### on the diagonal
  res$intermediate$ggpairs.normal <-
    ggpairs(res$intermediate$dat, diag=list(continuous="barDiag", discrete="barDiag"),
            axisLabels=res$input$axisLabels);
#  lower="blank",
  
  ### Then generate one with jittered points
  res$intermediate$ggpairs.jittered <-
    ggpairs(res$intermediate$dat,
            diag=list(continuous="blankDiag"),
            upper=list(continuous=GGally::wrap("cor")),
            lower=list(continuous=GGally::wrap("points", position="jitter")),
            axisLabels=res$input$axisLabels);

  ### Copy the the one with the jittered points
  res$intermediate$ggpairs.combined <- res$intermediate$ggpairs.jittered;

  if (powerHist) {
    ### Create histograms and add them to the combined plot
    res$intermediate$powerHists <- list();
    for (currentVar in 1:length(items)) {
      res$intermediate$powerHists[[items[currentVar]]] <-
        powerHist(res$intermediate$dat[[items[currentVar]]], ...);
      res$intermediate$ggpairs.combined <-
        putPlot(res$intermediate$ggpairs.combined,
                res$intermediate$powerHists[[items[currentVar]]]$plot,
                currentVar, currentVar);
    }
  }
  else {
    ### Then place the histograms from the 'normal' one
    ### on the diagonal of the jittered scattermatrix
    for (currentVar in 1:length(items)) {
      res$intermediate$ggpairs.combined <-
        putPlot(res$intermediate$ggpairs.combined,
                getPlot(res$intermediate$ggpairs.normal, currentVar, currentVar),
                currentVar, currentVar);
    }
  }
  
  ### Copy combined matrix to the output for final adjustments
  res$output$scatterMatrix <- res$intermediate$ggpairs.combined;
  
  ### Adjust the size of the plots
  for (currentRowFromTop in 1:length(items)) {
    for (currentColumnFromLeft in 1:length(items)) {
      res$output$scatterMatrix <-
        putPlot(res$output$scatterMatrix,
                getPlot(res$output$scatterMatrix, currentRowFromTop, currentColumnFromLeft) + plotSettings,
                currentRowFromTop, currentColumnFromLeft);
    }
  }

  ### Set class and return result
  class(res) <- "scatterMatrix";
  return(res);
  
}

print.scatterMatrix <- function(x, ...) {
  ###
  print(x$output$scatterMatrix, ...);
}
