pwr.randomizationSuccess <- function(dNonequivalence = .2,
                                     pRandomizationSuccess = .95,
                                     nNuisanceVars = 100) {

  res <- sapply(dNonequivalence,
                function(dNonequival) {
                  return(sapply(pRandomizationSuccess, function(rRandSuccess, dNonequiv = dNonequival) {
                    return(sapply(nNuisanceVars, function(nNuisance, pSuccess = rRandSuccess, dNoneq = dNonequiv) {
                      n <- 10;
                      if (pdMild(dNoneq, n)^nNuisance > pSuccess) {
                        return(n)
                      } else {
                        while(pdMild(dNoneq, n)^nNuisance < pSuccess) {n <- n + 100;};
                        n <- n - 100;
                        while(pdMild(dNoneq, n)^nNuisance < pSuccess) {n <- n + 10;};
                        n <- n - 10;
                        while(pdMild(dNoneq, n)^nNuisance < pSuccess) {n <- n + 1;};
                        return(n);

                      }
                    }));
                  }));
                });

  res <- array(unclass(res), dim = c(length(nNuisanceVars),
                                     length(pRandomizationSuccess),
                                     length(dNonequivalence)),
               dimnames=list(paste("Nuisance var:", nNuisanceVars),
                             paste("Equival. prob:", pRandomizationSuccess),
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
