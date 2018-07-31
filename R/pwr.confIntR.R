#' Determine required sample size for a given confidence interval width for
#' Pearson's r
#' 
#' This function computes how many participants you need if you want to achieve
#' a confidence interval of a given width. This is useful when you do a study
#' and you are interested in how strongly two variables are associated.
#' 
#' 
#' @param r The correlation you expect to find (confidence intervals for a
#' given level of confidence get narrower as the correlation coefficient
#' increases).
#' @param w The required half-width (or margin of error) of the confidence
#' interval.
#' @param conf.level The level of confidence.
#' @return The required sample size, or a vector or matrix of sample sizes if
#' multiple correlation coefficients or required (half-)widths were supplied.
#' The row and column names specify the \code{r} and \code{w} values to which
#' the sample size in each cell corresponds. The confidence level is set as
#' attribute to the resulting vector or matrix.
#' @author Douglas Bonett (UC Santa Cruz, United States), with minor edits by
#' Murray Moinester (Tel Aviv University, Israel) and Gjalt-Jorn Peters (Open
#' University of the Netherlands, the Netherlands).
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{pwr.confIntR}}
#' @references Bonett, D. G., Wright, T. A. (2000). Sample size requirements
#' for estimating Pearson, Kendall and Spearman correlations.
#' \emph{Psychometrika, 65}, 23-28.
#' 
#' Bonett, D. G. (2014). CIcorr.R and sizeCIcorr.R
#' http://people.ucsc.edu/~dgbonett/psyc181.html
#' 
#' Moinester, M., & Gottfried, R. (2014). Sample size estimation for
#' correlations with pre-specified confidence interval. \emph{The Quantitative
#' Methods of Psychology, 10}(2), 124-130.
#' http://www.tqmp.org/RegularArticles/vol10-2/p124/p124.pdf
#' 
#' Peters, G. J. Y. & Crutzen, R. (forthcoming) An easy and foolproof method
#' for establishing how effective an intervention or behavior change method is:
#' required sample size for accurate parameter estimation in health psychology.
#' @keywords htest
#' @examples
#' 
#' pwr.confIntR(c(.4, .6, .8), w=c(.1, .2));
#' 
#' @export pwr.confIntR
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
