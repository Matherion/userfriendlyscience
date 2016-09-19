samplingDistribution <- function(popValues = c(0, 1), popFrequencies = c(50, 50),
                                 sampleSize = NULL, sampleFromPop = FALSE, ...) {
  
  if (is.null(sampleSize)) {
    sampleSize <- sum(popFrequencies);
  }

  if (sampleFromPop) {
    sampleVector <- sample(popValues, size=sampleSize,
                           replace=TRUE, prob=popFrequencies);
  }
  else {
    sampleVector <- rep(popValues, times=popFrequencies);    
  }
  
  return(normalityAssessment(sampleVector = sampleVector, ...));
  
}