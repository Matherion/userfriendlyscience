### From http://stats.stackexchange.com/questions/130069/what-is-the-distribution-of-r2-in-linear-regression-under-the-null-hypothesis

dRsq <- function(x, nPredictors, sampleSize, populationRsq = 0) {
  if (populationRsq != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  ### Return density for given R squared
  return(dbeta(x, (nPredictors-1)/2, (sampleSize - nPredictors) / 2));
}

pRsq <- function(q, nPredictors, sampleSize, populationRsq = 0, lower.tail=TRUE) {
  if (populationRsq != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  ### Return p-value for given R squared
  pValue <- pbeta(q, (nPredictors-1)/2, (sampleSize - nPredictors) / 2);
  return(ifelse(lower.tail, pValue, 1-pValue));
}

qRsq <- function(p, nPredictors, sampleSize, populationRsq = 0, lower.tail=TRUE) {
  if (populationRsq != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  p <- ifelse(lower.tail, p, 1-p);
  ### Return R squared for given p-value
  return(qbeta(1-p, (nPredictors-1)/2, (sampleSize - nPredictors) / 2));
}

rRsq <- function(n, nPredictors, sampleSize, populationRsq = 0) {
  if (populationRsq != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  ### Return random R squared value(s)
  return(rbeta(n, (nPredictors-1)/2, (sampleSize - nPredictors) / 2));
}
