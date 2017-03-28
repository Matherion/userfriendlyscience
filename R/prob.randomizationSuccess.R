prob.randomizationSuccess <- function(n = 1000,
                                      dNonequivalence = .2,
                                      nNuisanceVars = 100) {

  res <- sapply(dNonequivalence,
                function(dNonequival) {
                  return(sapply(n, function(nSize, dNonequiv = dNonequival) {
                    return(sapply(nNuisanceVars, function(nNuisance, sampleSize = nSize, dNoneq = dNonequiv) {
                      return(pdMild(dNoneq, sampleSize)^nNuisance);
                    }));
                  }));
                });
  
  res <- array(unclass(res), dim = c(length(nNuisanceVars),
                                     length(n),
                                     length(dNonequivalence)),
               dimnames=list(paste("Nuisance var:", nNuisanceVars),
                             paste("N:", n),
                             paste('Nonequival. at: d=', dNonequivalence)));
  
  if (sum(dim(res) == 1) > 1) {
    return(as.vector(res));
  } else if(sum(dim(res) == 1) == 1) {
    
    dims <- dim(res);
    dimNms <- dimnames(res);
    
    dimNms <- dimNms[dims > 1];
    dims <- dims[dims > 1];
    
    res <- matrix(res, ncol=dims[1],
                  dimnames=dimNms);
    
    return(res);
    
  } else {
    return(res);
  }
  
  
}
