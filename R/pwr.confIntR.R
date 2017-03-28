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
  res <- as.matrix(sapply(1:length(w), function(i) {
    currentW <- w[i]*2;
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

# pwr.confIntR <- function(r, w = .1, conf.level = .95) {
#
#
#   ### From a post at the R-help mailig list by Luke Tierney, see
#   ### http://stackoverflow.com/questions/3903157/how-can-i-check-whether-a-function-call-results-in-a-warning
#   wHandler <- function(w) {
#     myWarnings <<- c(myWarnings, list(w));
#     invokeRestart("muffleWarning");
#   }
#   myWarnings <- NULL;
#
#   rSign <- ifelse(r < 0, -1, 1);
#   r <- abs(r);
#   lowerP <- (1-conf.level) / 2;
#   upperP <- 1 - ((1-conf.level) / 2);
#   lowerBound <- abs(r) - abs(w);
#   upperBound <- abs(r) + abs(w);
#   n <- numeric();
#
#   for (ri in 1:length(r)) {
#     n[ri] <- 20;
#     withCallingHandlers(while (lowerBound[ri] > qPearson(lowerP, n[ri]-2, rho=r[ri], lower.tail=TRUE) ||
#                                upperBound[ri] < qPearson(upperP, n[ri]-2, rho=r[ri], lower.tail=FALSE)) {
#       n[ri] <- n[ri] + 100;
#     }, warning = wHandler);
#     n[ri] <- n[ri] - 100;
#     withCallingHandlers(while (lowerBound[ri] > qPearson(lowerP, n[ri]-2, rho=r[ri], lower.tail=TRUE) ||
#                                upperBound[ri] < qPearson(upperP, n[ri]-2, rho=r[ri], lower.tail=FALSE)) {
#       n[ri] <- n[ri] + 10;
#     }, warning = wHandler);
#     n[ri] <- n[ri] - 10;
#     withCallingHandlers(while (lowerBound[ri] > qPearson(lowerP, n[ri]-2, rho=r[ri], lower.tail=TRUE) ||
#                                upperBound[ri] < qPearson(upperP, n[ri]-2, rho=r[ri], lower.tail=FALSE)) {
#       n[ri] <- n[ri] + 1;
#     }, warning = wHandler);
#   }
#
#   return(n);
#
# }
