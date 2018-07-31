#' testRetestAlpha
#' 
#' The testRetestAlpha function computes the test-retest alpha coefficient
#' (Green, 2003).
#' 
#' 
#' This function computes the test-retest alpha coefficient as described in
#' Green (2003).
#' 
#' @param dat A dataframe containing the items in the scale at both measurement
#' moments.  If no dataframe is specified, a dialogue will be launched to allow
#' the user to select an SPSS datafile. If only one dataframe is specified,
#' either the items have to be ordered chronologically (i.e. first all items
#' for the first measurement, then all items for the second measurement), or
#' the vector 'moments' has to be used to indicate, for each item, to which
#' measurement moment it belongs.
#' @param moments Used to indicate to which measurement moment each item in
#' 'dat' belongs; should be a vector with the same length as dat has columns,
#' and with two possible values (e.g. 1 and 2).
#' @param testDat,retestDat Dataframes with the items for each measurement
#' moment: note that the items have to be in the same order (unless sortItems
#' is TRUE).
#' @param sortItems If true, the columns (items) in each dataframe are ordered
#' alphabetically before starting. This can be convenient to ensure that the
#' order of the items at each measurement moment is the same.
#' @param convertToNumeric When TRUE, the function will attempt to convert all
#' vectors in the dataframes to numeric.
#' @return
#' 
#' An object with the input and several output variables. Most notably:
#' \item{input}{Input specified when calling the function}
#' \item{intermediate}{Intermediate values and objects computed to get to the
#' final results} \item{output$testRetestAlpha}{The value of the test-retest
#' alpha coefficient.}
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @references Green, S. N. (2003). A Coefficient Alpha for Test-Retest Data.
#' Psychological Methods, 8(1), 88-101. doi:10.1037/1082-989X.8.1.88
#' @keywords utilities univar
#' @examples
#' 
#' 
#' \dontrun{
#' ### This will prompt the user to select an SPSS file
#' testRetestAlpha();
#' }
#' 
#' ### Load data from simulated dataset testRetestSimData (which
#' ### satisfies essential tau-equivalence).
#' data(testRetestSimData);
#' 
#' ### The first column is the true score, so it's excluded in this example.
#' exampleData <- testRetestSimData[, 2:ncol(testRetestSimData)];
#' 
#' ### Compute test-retest alpha coefficient
#' testRetestAlpha(exampleData);
#' 
#' 
#' @export testRetestAlpha
testRetestAlpha <- function(dat = NULL, moments = NULL,
                            testDat = NULL, retestDat = NULL,
                            sortItems = FALSE, convertToNumeric = TRUE) {
  
  res <- list(input = list(dat = dat,
                           moments = moments,
                           testDat = testDat,
                           retestDat = retestDat,
                           sortItems = sortItems,
                           convertToNumeric = convertToNumeric),
              intermediate = list(), output = list());
  
  ### If no dataframe was specified, load it from an SPSS file
  if (is.null(dat) && is.null(testDat) && is.null(retestDat)) {
    dat <- getData(errorMessage=paste0("No dataframe specified, and no valid datafile selected in ",
                                       "the dialog I then showed to allow selection of a dataset.",
                                       "Original error:\n\n[defaultErrorMessage]"),
                   use.value.labels=FALSE, applyRioLabels = FALSE);
  }
  
  if (!is.null(dat)) {
    if (is.null(res$intermediate$moments)) {
      res$intermediate$moments <- rep(c(0,1), each=(ncol(dat))/2);
    }
    momentsBoolean <- (res$intermediate$moments == min(res$intermediate$moments));
    res$intermediate$testDat <- testDat <- dat[, momentsBoolean];
    res$intermediate$retestDat <- retestDat <- dat[, !momentsBoolean];
  }
  else if (xor(is.null(testDat), is.null(retestDat))) {
    stop("Provide both testDat and retestDat; or, if you have all scores in one ",
         "dataframe, provide it as 'dat' argument!");
  }
  
  if (sortItems) {
    res$intermediate$testDat <- testDat <- testDat[, order(names(testDat))];
    res$intermediate$retestDat <- retestDat <- retestDat[, order(names(retestDat))];
  }

  if (convertToNumeric) {
    res$intermediate$testDat <- testDat <- massConvertToNumeric(testDat);
    res$intermediate$retestDat <- retestDat <- massConvertToNumeric(retestDat);
  }
  
  ### So now we have a testDat and a retestDat, so we can get started.
  
  ### First get the sum of the covariances between
  ### the different-time/different-item covariances (equation 14, page 94
  ### of Green, 2003).
  res$intermediate$covar <- cov(testDat, retestDat);
  ### We have to remove the sum of the variances of course.
  res$intermediate$covar.sum <-
    sum(res$intermediate$covar) - sum(diag(res$intermediate$covar));
  
  ### Divide by (J(J-1)), where J is the number of items (equation 14)
  res$intermediate$J <- J <- ncol(testDat);
  res$intermediate$itemTrueScoreVariance <-
    res$intermediate$covar.sum / (J * (J - 1));
  
  ### Get item true score variance (equation 15, which is the numerator
  ### in the test-retest alpha)
  res$intermediate$testTrueScoreVariance <-
    J^2 * res$intermediate$itemTrueScoreVariance;
  
  ### Get common scale variance (equation 16, which is the denominator
  ### in the test-retest alpha)
  
  ### First compute the scales themselves
  res$intermediate$testScale <- rowSums(testDat);
  res$intermediate$retestScale <- rowSums(retestDat);
  
  ### The the product of the standard deviations
  res$intermediate$commonScaleVariance <-
    sd(res$intermediate$testScale) * sd(res$intermediate$retestScale);
  
  ### Then compute the test-retest alpha coefficient
  res$output$testRetestAlpha <- res$intermediate$testTrueScoreVariance /
    res$intermediate$commonScaleVariance;
  
  class(res) <- 'testRetestAlpha';
  return(res);
  
}

print.testRetestAlpha <- function(x, ...) {
  print(x$output$testRetestAlpha, ...);
}
