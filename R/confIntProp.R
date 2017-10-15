confIntProp <- function(x, n, conf.level = .95) {
  
  originalN <- n;
  originalX <- x;
  
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
    return(sapply(seq_along(originalX), function(i) {
      ### Then for n
      return(sapply(seq_along(originalN), function(j) {
        return(c(x[i] / n[j],
                 p.L(x=x[i],
                     n=n[j],
                     alpha=(1 - confLevel)/2),
                 p.U(x=x[i],
                     n=n[j],
                     alpha=(1 - confLevel)/2)));
      }));
    }));
  });
  if (ncol(res) > 1) {
    res <- lapply(1:ncol(res), function(confI) {
      rslt <- matrix(res[, confI], ncol=3, byrow=TRUE);
      rownames(rslt) <- paste0(rslt[, 1], ", ",
                               rep(100*conf.level[confI], each=nrow(rslt)), "%");
      rslt <- rslt[, -1];
      colnames(rslt) <- c('ci.lo', 'ci.hi');
      return(rslt);
    });
    res <- do.call(rbind, res);
  } else {
    res <- matrix(res, ncol=3, byrow=TRUE);
    rownames(res) <- paste0(res[, 1], ", ",
                            rep(100*conf.level, each=nrow(res)), "%");
    res <- res[, -1, drop=FALSE];
    colnames(res) <- c('ci.lo', 'ci.hi');
  }
  return(res);
}
