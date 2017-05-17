confIntR <- function(r, N, conf.level = .95) {
  if ((r < -1) || (r > 1)) {
    stop("The specified observed correlation (argument 'r') must be between -1 and 1.");
  }
  if ((conf.level < 0.0001) || (conf.level >= 1)) {
    stop("The specified desired confidence level (argument 'conf.level') must be between 0 and 1.");
  }
  if (any(N < 4)) {
    stop("The specified sample size (argument 'N') must be at least 4.");
  }
  if ((length(r) > 1) && (length(N) > 1)) {
    stop("Sorry, current implementation only vectorized over either r of N!");
  }
  Z <- qnorm(1 - (1-conf.level)/2);
  se <- sqrt(1/((N - 3)));
  zr <- log((1 + r)/(1 - r))/2;
  LL0 <- zr - Z*se;
  UL0 <- zr + Z*se;
  LL <- (exp(2*LL0) - 1)/(exp(2*LL0) + 1);
  UL <- (exp(2*UL0) - 1)/(exp(2*UL0) + 1);
  CI2w <- UL - LL;
  CI <- matrix(c(LL, UL), byrow=FALSE, ncol=2);
  rownames(CI) <- ifelseObj(length(N) > 1, N, r);
  colnames(CI) <- c('lo', 'hi');
  attr(CI, 'r') <- r;
  attr(CI, 'N') <- N;
  attr(CI, 'conf.level') <- conf.level;
  return(CI);
}
