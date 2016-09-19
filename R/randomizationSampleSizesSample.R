randomizationSampleSizesSample <- function(sampleSize, nVars,
                                           nClusters = 1,
                                           rMean = 0) {
  
  if (rMean == 0) {
    
    confounderValues <- matrix(rnorm(sampleSize * nVars),
                               ncol = nVars);
    
  } else {
    
    ### Compute number of variables in each cluster
    clusterSize <- nVars / nClusters;
    
    ### Simulate values for the confounders in that number of clusters
    confounderValues <-
      matrix(sapply(1:nClusters, mvrnorm,
                    n = sampleSize,
                    mu = rep(0, clusterSize),
                    Sigma = createSigma(nVar = clusterSize,
                                        meanR = rMean)),
             ncol = nVars);
    
  }
  
  ### Randomly create two groups
  groupVector <- factor(round(runif(sampleSize)));
  
  ### Compute Cohen's d values for each confounder
  cohensDs <-
    apply(confounderValues, 2,
          function(currentConfounder, group = groupVector) {
            return((mean(currentConfounder[group == 0]) - 
                      mean(currentConfounder[group == 1])) /
                     sd(currentConfounder));
          });
  
  if (nVars == 1) {
    names(cohensDs) <- paste0("sampleSize=", sampleSize);
  } else {
    names(cohensDs) <- paste0("confounder", 1:nVars);
  }
  
  return(cohensDs);
  
}
