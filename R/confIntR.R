#' A function to compute a correlation's confidence interval
#' 
#' This function computes the confidence interval for a given correlation and
#' its sample size. This is useful to obtain confidence intervals for
#' correlations reported in papers when informing power analyses.
#' 
#' 
#' @param r The observed correlation coefficient.
#' @param N The sample size of the sample where the correlation was computed.
#' @param conf.level The desired confidence level of the confidence interval.
#' @param plot Whether to show a plot.
#' @return The confidence interval(s) in a matrix with two columns. The left
#' column contains the lower bound, the right column the upper bound. The
#' \code{\link{rownames}} are the observed correlations, and the
#' \code{\link{colnames}} are 'lo' and 'hi'. The confidence level and sample
#' size are stored as attributes. The results are returned like this to make it
#' easy to access single correlation coefficients from the resulting object
#' (see the examples).
#' @author Douglas Bonett (UC Santa Cruz, United States), with minor edits by
#' Murray Moinester (Tel Aviv University, Israel) and Gjalt-Jorn Peters (Open
#' University of the Netherlands, the Netherlands).
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{confIntR}}
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
#'   
#'   ### To request confidence intervals for one correlation
#'   confIntR(.3, 100);
#'   
#'   ### The lower bound of a single correlation
#'   confIntR(.3, 100)[1];
#' 
#'   ### To request confidence intervals for multiple correlations:
#'   confIntR(c(.1, .3, .5), 250);
#'   
#'   ### The upper bound of the correlation of .5:
#'   confIntR(c(.1, .3, .5), 250)['0.5', 'hi'];
#' 
#' 
#' @export confIntR
confIntR <- function(r, N, conf.level = .95, plot=FALSE) {
  if ((r < -1) || (r > 1)) {
    stop("The specified observed correlation (argument 'r') must be between -1 and 1.");
  }
  if ((conf.level < 0.000001) || (conf.level >= 1)) {
    stop("The specified desired confidence level (argument 'conf.level') must be between .000001 and 1.");
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
  res <- matrix(c(LL, UL), byrow=FALSE, ncol=2);
  rownames(res) <- ifelseObj(length(N) > 1, N, r);
  colnames(res) <- c('lo', 'hi');
  attr(res, 'r') <- r;
  attr(res, 'N') <- N;
  attr(res, 'conf.level') <- conf.level;

  if (plot) {
    if ((length(r) > 1) || (length(N) > 1) || (length(conf.level) > 1)) {
      warning("I can only produce a plot if you supply only one value for ",
              "arguments r, N, and conf.level!");
    } else {
      df <- data.frame(r = seq(-1, 1, .001));
      
      ### From a post at the R-help mailig list by Luke Tierney, see
      ### http://stackoverflow.com/questions/3903157/how-can-i-check-whether-a-function-call-results-in-a-warning
      wHandler <- function(w) {
        myWarnings <<- c(myWarnings, list(w));
        invokeRestart("muffleWarning");
      }
      myWarnings <- NULL;        
      
      df$density <- withCallingHandlers(SuppDists::dPearson(df$r, N = N, rho = r),
                                        warning = wHandler);
      
      cilo <- min(res);
      cihi <- max(res);
      rValue <- r;
      
      plot <- ggplot(df, aes(x=r, y=density)) +
        theme_bw() +
        theme(axis.title.x.top = element_blank()) +
        scale_x_continuous(sec.axis = dup_axis(breaks=c(cilo,
                                                        rValue,
                                                        cihi),
                                               labels=round(c(cilo,
                                                              rValue,
                                                              cihi), 2))) +
        geom_vline(aes(xintercept=cilo), linetype='dashed') +
        geom_vline(aes(xintercept=rValue), linetype='dashed') +
        geom_vline(aes(xintercept=cihi), linetype='dashed') +
        geom_ribbon(data=df[df$r >= min(res) & df$r <= max(res), ],
                    aes(ymin = 0, ymax=density),
                    fill='#cadded') +
        geom_segment(x = min(res),
                     xend = min(res),
                     y = 0,
                     yend = SuppDists::dPearson(min(res), N = N,
                                                rho = r),
                     color = '#2a5581', size=1.5) +
        geom_segment(x = max(res),
                     xend = max(res),
                     y = 0,
                     yend = SuppDists::dPearson(max(res), N = N,
                                                rho = r),
                     color = '#2a5581', size=1.5) +
        geom_line(size=1.5);
      
      attr(res, "plot") <- plot;
      class(res) <- 'confIntR';
    }
  }
  
  return(res);
}

print.confIntR <- function(x, ...) {
  ### Basically a trick because we're passing the plot as an attribute.
  if (!is.null(attr(x, 'plot'))) {
    grid.draw(attr(x, 'plot'));
    ### So remove the plot
    attr(x, 'plot') <- NULL;
  }
  ### And then remove the class to print
  print(unclass(x));
}

