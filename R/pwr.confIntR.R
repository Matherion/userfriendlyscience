pwr.confIntR <- function(r, w = .1, conf.level = .95) {
  if ((r < -1) || (r > 1)) {
    stop("The specified observed correlation (argument 'r') must be between -1 and 1.");
  }
  if ((conf.level < 0.01) || (conf.level >= 1)) {
    stop("The specified desired confidence level (argument 'conf.level') must be between 0 and 1.");
  }
  if ((w <= 0) || (w >= 1)) {
    stop("The specified desired confidence level halfwidth (argument 'w') must be between 0 and 1.");
  }
  res <- as.matrix(sapply(1:length(w), function(currentW) {
    currentW <- w[currentW]*2;
    z <- qnorm(1 - (1-conf.level)/2);
    n1 <- ceiling(4*(1 - r^2)^2*(z/currentW)^2 + 3);
    zr <- log((1 + r)/(1 - r))/2;
    se <- sqrt(1/(n1 - 3));
    LL0 <- zr - z*se;
    UL0 <- zr + z*se;
    LL <- (exp(2*LL0) - 1)/(exp(2*LL0) + 1);
    UL <- (exp(2*UL0) - 1)/(exp(2*UL0) + 1);
    return(ceiling((n1 - 3)*((UL - LL)/currentW)^2 + 3));
  }));
  colnames(res) <- paste0('w = ', w);
  rownames(res) <- paste0('r = ', r);
  attr(res, 'conf.level') <- conf.level;
  return(res);
}
