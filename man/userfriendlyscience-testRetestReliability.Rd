\name{testRetestReliability}
\alias{testRetestReliability}
\title{
testRetestReliability

}
\description{
The testRetestReliability function is a convenient interface to testRetestAlpha and testRetestCES.
}
\usage{
testRetestReliability(dat = NULL, moments = NULL,
                      testDat = NULL, retestDat = NULL,
                      parallelTests = 'means',
                      sortItems = FALSE, convertToNumeric = TRUE,
                      digits=2)
}
\arguments{
  \item{dat}{
  A dataframe. This dataframe must contain the items in
  the scale at both measurement moments. If no dataframe is specified, a
  dialogue will be launched to allow the user
  to select an SPSS datafile. If only one dataframe is specified, either
  the items have to be ordered chronologically (i.e. first all items for the
  first measurement, then all items for the second measurement), or the
  vector 'moments' has to be used to indicate, for each item, to which
  measurement moment it belongs. The number of columns in this dataframe
  MUST be even! Note that instead of providing this dataframe, the items
  of each measurement moment can be provided separately in testDat and
  retestDat as well.
}
  \item{moments}{
  Used to indicate to which measurement moment each item in 'dat' belongs;
  should be a vector with the same length as dat has columns, and with two
  possible values (e.g. 1 and 2).
}
  \item{testDat, retestDat}{
  Dataframes with the items for each measurement moment: note that the
  items have to be in the same order (unless sortItems is TRUE).
}
  \item{parallelTests}{
  A vector indicating which items belong to which parallel test; like the
  moments vector, this should have two possible values (e.g. 1 and 2).
  Alternatively, it can be character value with 'means' or 'variances';
  in this case, parallelSubscales will be used to create roughly
  parallel halves.
}
  \item{sortItems}{
  If true, the columns (items) in each dataframe are ordered
  alphabetically before starting. This can be convenient to ensure that
  the order of the items at each measurement moment is the same.
}
  \item{convertToNumeric}{
  When TRUE, the function will attempt to convert all vectors in the dataframes
  to numeric.
}
  \item{digits}{
  Number of digits to show when printing the output
}
}
\details{

This function calls both testRetestAlpha and testRetestCES to compute and print measures of the test-retest reliability.

}
\value{

An object with the input and several output variables. Most notably:
  \item{input}{Input specified when calling the function}
  \item{intermediate}{Intermediate values and objects computed to get to the final results}
  \item{output$testRetestAlpha}{The value of the test-retest alpha coefficient.}
  \item{output$testRetestCES}{The value of the test-retest Coefficient of Equivalence and Stability.}
}
\examples{

\dontrun{
### This will prompt the user to select an SPSS file
testRetestReliability();
}

### Load data from simulated dataset testRetestSimData (which
### satisfies essential tau-equivalence).
data(testRetestSimData);

### The first column is the true score, so it's excluded in this example.
exampleData <- testRetestSimData[, 2:ncol(testRetestSimData)];

### Compute test-retest alpha coefficient
testRetestReliability(exampleData);

}
\keyword{ utilities }
\keyword{ univar }