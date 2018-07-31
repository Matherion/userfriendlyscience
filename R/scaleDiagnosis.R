###########################################################
###########################################################
###
### Function to generate an object with several useful
### statistics and a plot to assess how the elements
### (usually items) in a scale relate to each other.
###
### File created by Gjalt-Jorn Peters. Questions? You can
### contact me through http://behaviorchange.eu.
###
###########################################################
###########################################################

### Scale Diagnosis


#' scaleDiagnosis
#' 
#' scaleDiagnosis provides a number of diagnostics for a scale (an aggregative
#' measure consisting of several items).
#' 
#' 
#' Function to generate an object with several useful statistics and a plot to
#' assess how the elements (usually items) in a scale relate to each other,
#' such as Cronbach's Alpha, omega, the Greatest Lower Bound, a factor
#' analysis, and a correlation matrix.
#' 
#' @param dat A dataframe containing the items in the scale. All variables in
#' this dataframe will be used if items is NULL.
#' @param items If not NULL, this should be a character vector with the names
#' of the variables in the dataframe that represent items in the scale.
#' @param plotSize Size of the final plot in millimeters.
#' @param sizeMultiplier Allows more flexible control over the size of the plot
#' elements
#' @param axisLabels Passed to ggpairs function to set axisLabels.
#' @param scaleReliability.ci TRUE or FALSE: whether to compute confidence
#' intervals for Cronbach's Alpha and Omega (uses bootstrapping function in
#' MBESS, takes a while).
#' @param conf.level Confidence of confidence intervals for reliability
#' estimates (if requested with scaleReliability.ci).
#' @param powerHist Whether to use the default ggpairs histogram on the
#' diagonal of the scattermatrix, or whether to use the powerHist version.
#' @param ...  Additional arguments are passed on to powerHist.
#' @return
#' 
#' An object with the input and several output variables. Most notably:
#' \item{scaleReliability}{The results of scaleReliability.} \item{pca}{A
#' Principal Components Analysis} \item{fa}{A Factor Analysis}
#' \item{describe}{Decriptive statistics about the items}
#' \item{scatterMatrix}{A scattermatrix with histograms on the diagonal and
#' correlation coefficients in the upper right half.}
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utilities univar
#' @examples
#' 
#' ### Note: the 'not run' is simply because running takes a lot of time,
#' ###       but these examples are all safe to run!
#' \dontrun{
#' ### This will prompt the user to select an SPSS file
#' scaleDiagnosis();
#' 
#' ### Generate a datafile to use
#' exampleData <- data.frame(item1=rnorm(100));
#' exampleData$item2 <- exampleData$item1+rnorm(100);
#' exampleData$item3 <- exampleData$item1+rnorm(100);
#' exampleData$item4 <- exampleData$item2+rnorm(100);
#' exampleData$item5 <- exampleData$item2+rnorm(100);
#' 
#' ### Use a selection of two variables
#' scaleDiagnosis(dat=exampleData, items=c('item2', 'item4'));
#' 
#' ### Use all items
#' scaleDiagnosis(dat=exampleData);
#' }
#' 
#' @export scaleDiagnosis
scaleDiagnosis <- function(dat=NULL, items=NULL, plotSize=180, sizeMultiplier = 1,
                           axisLabels = "none", scaleReliability.ci=FALSE,
                           conf.level=.95, powerHist=TRUE, ...) {

  ### If no dataframe was specified, load it from an SPSS file
  if (is.null(dat)) {
    dat <- getData(errorMessage=paste0("No dataframe specified, and no valid SPSS file selected in ",
                                       "the dialog I then showed to allow selection of a dataset."),
                   use.value.labels=FALSE);
  }
  else {
    if (!is.data.frame(dat)) {
      stop("Argument 'dataframe' must be a dataframe or NULL! Class of ",
           "provided argument: ", class(dat));
    }
  }
  
  if (is.null(items)) {
    items <- names(dat);
  }
  
  ### Create object to store results
  res <- list();
  res$items <- items;
  res$plotSize <- plotSize;
  res$sizeMultiplier <- sizeMultiplier;
  
  ### Extract dataframe and select only complete cases
  res$dat <- dat[complete.cases(dat[, items]), items];
  res$n <- nrow(res$dat);
  
  ### Convert all variables to numeric vectors, if they weren't already
  res$dat <- data.frame(lapply(res$dat, 'as.numeric'));
  
  ### Basic univariate descriptives
  res$describe <- describe(res$dat);
  
  ### Bivariate correlations
  res$cor <- cor(res$dat, use="complete.obs");

  res$scatterMatrix <- scatterMatrix(res$dat, plotSize=180, sizeMultiplier = 1,
                                     axisLabels = "none", powerHist=powerHist, ...);
                                       
  
  ### Exploratory factor analysis
  #pa.out <- factor.pa(r = bfi, nfactors = 5, residuals = FALSE,
  #                    + rotate = "varimax", n.obs = NA, scores = FALSE, SMC = TRUE,
  #                    + missing = FALSE, impute = "median", min.err = 0.001, digits = 2,
  #                    + max.iter = 100, symmetric = TRUE, warnings = TRUE, fm = "pa")
  
  ### Extract eigen values
  res$eigen <- eigen(res$cor);
  ### Determine how many factors have eigenvalues
  ### over 1 - note that we're not doing a real
  ### exploratory factor analysis, we're just interested
  ### in whether this scale works out (it's not
  ### unidimensional if more than one factor has an
  ### eigenvalue a lot over 1)
  res$factors <- sum(res$eigen$values > 1);
  
  ### If there are more than two items, do a principal
  ### component analysis and a factor analysis
  if (ncol(res$cor) > 2) {
    ### Principal components analysis
    res$pca <- principal(r = res$cor, n.obs = res$n, rotate="oblimin",
                         nfactors=res$factors);
    ### Exploratory factor analysis
    res$fa <- fa(r = res$cor, n.obs = res$n, rotate="oblimin",
                 fm="ml", nfactors=res$factors);
  }
  
  ### Internal consistency measures
  res$scaleReliability <- scaleStructure(dat=res$dat, items=items,
                                         ci=scaleReliability.ci,
                                         conf.level=conf.level);
  
  ### Return results
  class(res) <- c('scaleDiagnosis');
  return(res);
}

print.scaleDiagnosis <- function(x, ...) {
  print(x$scaleReliability, ...);
  cat(paste0("\nEigen values: ", paste(round(x$eigen$values, 3), collapse=", ")));
  print(x$pca$loadings, ...);
  cat("\n");
  print(x$describe, ...);
  print(x$scatterMatrix$output$scatterMatrix, ...);
  invisible();
}
