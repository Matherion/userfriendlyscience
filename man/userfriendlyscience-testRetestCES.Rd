\name{testRetestCES}
\alias{testRetestCES}
\alias{parallelSubscales}
\title{
testRetestCES

}
\description{
The testRetestCES function computes the test-retest Coefficient of Equivalence and Stability (Schmidt, Le & Ilies, 2003).
}
\usage{
testRetestCES(dat = NULL, moments = NULL,
              testDat = NULL, retestDat = NULL,
              parallelTests = 'means',
              sortItems = FALSE, convertToNumeric = TRUE,
              digits=4)
parallelSubscales(dat, convertToNumeric = TRUE)
}
\arguments{
  \item{dat}{
  A dataframe. For  testRetestCES, this dataframe must contain the items in
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
  Number of digits to print.
}
}
\details{

This function computes the test-retest Coefficient of Equivalence and Stability (CES) as described in
Schmidt, Le & Ilies (2003). Note that this function only computes the test-retest CES for a scale that
is administered twice and split into two parallel halves post-hoc (this procedure is explained on page
210, and the equations that are used, 16 and 17a are explained on page 212).

}
\value{

An object with the input and several output variables. Most notably:
  \item{input}{Input specified when calling the function}
  \item{intermediate}{Intermediate values and objects computed to get to the final results}
  \item{output$testRetestCES}{The value of the test-retest Coefficient of Equivalence and Stability.}
}
\references{
Schmidt, F. L., Le, H., & Ilies, R. (2003) Beyong Alpha: An Empirical Examination of the Effects of Different Sources of Measurement Error on Reliability Estimates for Measures of Individual-differences Constructs. Psychological Methods, 8(2), 206-224. doi:10.1037/1082-989X.8.x.206
}
\note{
This function uses equations 16 and 17 on page 212 of Schmidt, Le & Ilies (2003): in other words, this function assumes that one scale is administered twice. If you'd like the computation for two different but parellel scales/measures to be implemented, please contact me.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{

\dontrun{
### This will prompt the user to select an SPSS file
testRetestCES();
}

### Load data from simulated dataset testRetestSimData (which
### satisfies essential tau-equivalence).
data(testRetestSimData);

### The first column is the true score, so it's excluded in this example.
exampleData <- testRetestSimData[, 2:ncol(testRetestSimData)];

### Compute test-retest alpha coefficient
testRetestCES(exampleData);

}
\keyword{ utilities }
\keyword{ univar }