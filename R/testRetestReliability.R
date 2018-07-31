#' testRetestReliability
#' 
#' The testRetestReliability function is a convenient interface to
#' testRetestAlpha and testRetestCES.
#' 
#' 
#' This function calls both testRetestAlpha and testRetestCES to compute and
#' print measures of the test-retest reliability.
#' 
#' @param dat A dataframe. This dataframe must contain the items in the scale
#' at both measurement moments. If no dataframe is specified, a dialogue will
#' be launched to allow the user to select an SPSS datafile. If only one
#' dataframe is specified, either the items have to be ordered chronologically
#' (i.e. first all items for the first measurement, then all items for the
#' second measurement), or the vector 'moments' has to be used to indicate, for
#' each item, to which measurement moment it belongs. The number of columns in
#' this dataframe MUST be even! Note that instead of providing this dataframe,
#' the items of each measurement moment can be provided separately in testDat
#' and retestDat as well.
#' @param moments Used to indicate to which measurement moment each item in
#' 'dat' belongs; should be a vector with the same length as dat has columns,
#' and with two possible values (e.g. 1 and 2).
#' @param testDat,retestDat Dataframes with the items for each measurement
#' moment: note that the items have to be in the same order (unless sortItems
#' is TRUE).
#' @param parallelTests A vector indicating which items belong to which
#' parallel test; like the moments vector, this should have two possible values
#' (e.g. 1 and 2).  Alternatively, it can be character value with 'means' or
#' 'variances'; in this case, parallelSubscales will be used to create roughly
#' parallel halves.
#' @param sortItems If true, the columns (items) in each dataframe are ordered
#' alphabetically before starting. This can be convenient to ensure that the
#' order of the items at each measurement moment is the same.
#' @param convertToNumeric When TRUE, the function will attempt to convert all
#' vectors in the dataframes to numeric.
#' @param digits Number of digits to show when printing the output
#' @return
#' 
#' An object with the input and several output variables. Most notably:
#' \item{input}{Input specified when calling the function}
#' \item{intermediate}{Intermediate values and objects computed to get to the
#' final results} \item{output$testRetestAlpha}{The value of the test-retest
#' alpha coefficient.} \item{output$testRetestCES}{The value of the test-retest
#' Coefficient of Equivalence and Stability.}
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utilities univar
#' @examples
#' 
#' 
#' \dontrun{
#' ### This will prompt the user to select an SPSS file
#' testRetestReliability();
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
#' testRetestReliability(exampleData);
#' 
#' 
#' @export testRetestReliability
testRetestReliability <- function(dat = NULL, moments = NULL,
                                  testDat = NULL, retestDat = NULL,
                                  parallelTests = 'means',
                                  sortItems = FALSE, convertToNumeric = TRUE,
                                  digits=2) {

  ### Make object to store results
  res <- list(input = list(dat = dat,
                           moments = moments,
                           testDat = testDat,
                           retestDat = retestDat,
                           parallelTests = parallelTests,
                           sortItems = sortItems,
                           convertToNumeric = convertToNumeric,
                           digits = digits),
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
      if (is.odd(ncol(dat))) {
        stop("If argument 'dat' is provided, this dataframe is split into two. ",
             "Therefore, it must have an even number of columns.");
      }
      res$intermediate$moments <- rep(c(0,1), each=(ncol(dat))/2);
    }
    res$intermediate$momentsBoolean <- (res$intermediate$moments == min(res$intermediate$moments));
    res$intermediate$testDat <- testDat <- dat[, res$intermediate$momentsBoolean];
    res$intermediate$retestDat <- retestDat <- dat[, !res$intermediate$momentsBoolean];
  }
  else if (xor(is.null(testDat), is.null(retestDat))) {
    stop("Provide both testDat and retestDat; or, if you have all scores in one ",
         "dataframe, provide it as 'dat' argument!");
  }

  ### Store number of observations
  res$input$n.observations <- nrow(testDat);
  
  if (sortItems) {
    res$intermediate$testDat <- testDat <- testDat[, order(names(testDat))];
    res$intermediate$retestDat <- retestDat <- retestDat[, order(names(retestDat))];
  }
  
  if (ncol(testDat) != ncol(retestDat)) {
    stop("The dataframe for each measurement moment must have the same number of ",
         "items. The current version of testRetestCES only supports compuring the ",
         "test-retest CES for a scale that is split into parallel halves post-hoc; ",
         "see Schmidt, Le & Ilies (2003), pages 210 and 212.");
  }
  
  if (ncol(testDat) < 2) {
    stop("The scale at each measurement moment must contain at least two ",
         "items to split into subscales. The scale you specified has only ",
         ncol(testDat), " items.");
  }
  
  if (convertToNumeric) {
    res$intermediate$testDat <- testDat <- massConvertToNumeric(testDat);
    res$intermediate$retestDat <- retestDat <- massConvertToNumeric(retestDat);
  }
  
  ### So, now we have testDat with the data from the first administration,
  ### and retestDat with the data from the second administration. We can
  ### now call testRetestAlpha and testRetestCES.
  
  res$intermediate$testRetestAlpha <-
    testRetestAlpha(testDat = testDat, retestDat = retestDat);
  res$intermediate$testRetestCES <-
    testRetestCES(testDat = testDat, retestDat = retestDat,
                  parallelTests = parallelTests);
  
  ### Extract test-retest alpha and CES and store it in the output object
  res$output$testRetestAlpha <- res$intermediate$testRetestAlpha$output$testRetestAlpha;
  res$output$testRetestCES <- res$intermediate$testRetestCES$output$testRetestCES;
  
  class(res) <- "testRetestReliability";
  ### Return result
  return(res);
}

print.testRetestReliability <- function (x, digits=x$input$digits, ...) {
  cat(paste0("                         Items at time 1: ", paste(names(x$intermediate$testDat), collapse=", "),
           "\n                         Items at time 2: ", paste(names(x$intermediate$retestDat), collapse=", "),
           "\n                            Observations: ", x$input$n.observations,
           "\n           Test-retest Alpha Coefficient: ", round(x$output$testRetestAlpha, digits=digits),
           "\n"));
  print(x$intermediate$testRetestCES, digits=digits);
  invisible();
}
