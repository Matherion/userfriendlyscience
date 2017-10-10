confIntProp <- function(x, n, conf.level = .95) {
  if (length(x) > length(n)) {
    if (length(n) == 1) {
      n <- rep(n, length(x));
    } else {
      stop("Length of 'x' is ", length(x),
           ", but length of 'n' is ", length(n), ".");
    }
  } else if (length(x) < length(n)) {
    if (length(x) == 1) {
      x <- rep(x, length(n));
    } else {
      stop("Length of 'x' is ", length(x),
           ", but length of 'n' is ", length(n), ".");
    }
  }
  
  ### Adapted from binom.test
  p.L <- function(x, n, alpha) {
    if (x == 0) 
      0
    else qbeta(alpha, x, n - x + 1)
  }
  p.U <- function(x, n, alpha) {
    if (x == n) 
      1
    else qbeta(1 - alpha, x + 1, n - x)
  }
  ### END binom.test code

  ### Vectorized both for x/n pairs and for confidence levels;
  ### first process for each confidence level
  res <- sapply(conf.level, function(confLevel) {
    ### Then for x/n pairs for this given confidence level
    return(sapply(seq_along(x), function(i, confLvl = confLevel) {
      return(c(p.L(x=x[i],
                   n=n[i],
                   alpha=(1 - confLvl)/2),
               p.U(x=x[i],
                   n=n[i],
                   alpha=(1 - confLvl)/2)));
    }));
  });
  res <- matrix(res, ncol=2, byrow=TRUE);
  rownames(res) <- paste0(paste(x / n), ", ",
                          rep(100*conf.level, each=length(x)), "%");
  colnames(res) <- c('ci.lo', 'ci.hi');
  return(res);
}
