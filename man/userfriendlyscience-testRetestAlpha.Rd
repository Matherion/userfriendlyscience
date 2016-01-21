\name{testRetestAlpha}
\alias{testRetestAlpha}
\title{
testRetestAlpha
}
\description{
The testRetestAlpha function computes the test-retest alpha coefficient (Green, 2003).
}
\usage{
testRetestAlpha(dat = NULL, moments = NULL,
                testDat = NULL, retestDat = NULL,
                sortItems = FALSE, convertToNumeric = TRUE)
}
\arguments{
  \item{dat}{
  A dataframe containing the items in the scale at both measurement moments.
  If no dataframe is specified, a dialogue will be launched to allow the user
  to select an SPSS datafile. If only one dataframe is specified, either
  the items have to be ordered chronologically (i.e. first all items for the
  first measurement, then all items for the second measurement), or the
  vector 'moments' has to be used to indicate, for each item, to which
  measurement moment it belongs.
  
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
  \item{sortItems}{
  If true, the columns (items) in each dataframe are ordered
  alphabetically before starting. This can be convenient to ensure that
  the order of the items at each measurement moment is the same.
}
  \item{convertToNumeric}{
  When TRUE, the function will attempt to convert all vectors in the dataframes
  to numeric.
}
}
\details{

This function computes the test-retest alpha coefficient as described in
Green (2003).

}
\value{

An object with the input and several output variables. Most notably:
  \item{input}{Input specified when calling the function}
  \item{intermediate}{Intermediate values and objects computed to get to the final results}
  \item{output$testRetestAlpha}{The value of the test-retest alpha coefficient.}
}
\references{
Green, S. N. (2003). A Coefficient Alpha for Test-Retest Data. Psychological Methods, 8(1), 88-101. doi:10.1037/1082-989X.8.1.88
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}

\examples{

\dontrun{
### This will prompt the user to select an SPSS file
testRetestAlpha();
}

### Load data from simulated dataset testRetestSimData (which
### satisfies essential tau-equivalence).
data(testRetestSimData);

### The first column is the true score, so it's excluded in this example.
exampleData <- testRetestSimData[, 2:ncol(testRetestSimData)];

### Compute test-retest alpha coefficient
testRetestAlpha(exampleData);

}
\keyword{ utilities }
\keyword{ univar }