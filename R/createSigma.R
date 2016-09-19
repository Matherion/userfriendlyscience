createSigma <- function(nVar, meanR = .3, sdR = 0, diagonal = 1) {
  Sigma <- matrix(rnorm(n = nVar^2,
                        mean = meanR,
                        sd = sdR),
                  ncol = nVar);
  Sigma[(Sigma < -1) | (Sigma > 1)] <- 1;
  if (!is.null(diagonal)) {
    diag(Sigma) <- diagonal;
  }
  return(Sigma);
}
