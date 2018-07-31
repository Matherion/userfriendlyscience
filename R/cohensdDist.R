#' The distribution of Cohen's \emph{d}
#' 
#' These functions use some conversion to and from the \emph{t} distribution to
#' provide the Cohen's \emph{d} distribution. There are four versions that act
#' similar to the standard distribution functions (the \code{d.}, \code{p.},
#' \code{q.}, and \code{r.} functions, and their longer aliases
#' \code{.Cohensd}), three convenience functions (\code{pdExtreme},
#' \code{pdMild}, and \code{pdInterval}), a function to compute the confidence
#' interval for a Cohen's \emph{d} estimate \code{cohensdCI}, and a function to
#' compute the sample size required to obtain a confidence interval around a
#' Cohen's \emph{d} estimate with a specified accuracy (\code{pwr.cohensdCI}
#' and its alias \code{pwr.confIntd}).
#' 
#' The functions use \code{\link{convert.d.to.t}} and
#' \code{\link{convert.t.to.d}} to provide the Cohen's \emph{d} distribution.
#' 
#' More details about \code{cohensdCI} and \code{pwr.cohensdCI} are provided in
#' Peters & Crutzen (2017).
#' 
#' @aliases dCohensd pCohensd qCohensd rCohensd dd pd qd rd pdExtreme pdMild
#' pdInterval cohensdCI confIntD pwr.cohensdCI pwr.confIntd
#' @param x,q,d Vector of quantiles, or, in other words, the value(s) of
#' Cohen's \emph{d}.
#' @param ds A vector with two Cohen's \emph{d} values.
#' @param p Vector of probabilites (\emph{p}-values).
#' @param df Degrees of freedom.
#' @param n Desired number of Cohen's \emph{d} values for \code{rCohensd} and
#' \code{rd}, and the number of participants/datapoints for \code{pdExtreme},
#' \code{pdMild}, \code{pdInterval}, and \code{cohensdCI}.
#' @param populationD The value of Cohen's \emph{d} in the population; this
#' determines the center of the Cohen's \emph{d} distribution. I suppose this
#' is the noncentrality parameter.
#' @param lower.tail logical; if TRUE (default), probabilities are the
#' likelihood of finding a Cohen's \emph{d} smaller than the specified value;
#' otherwise, the likelihood of finding a Cohen's \emph{d} larger than the
#' specified value.
#' @param conf.level The level of confidence of the confidence interval.
#' @param plot Whether to show a plot of the sampling distribution of Cohen's
#' \emph{d} and the confidence interval. This can only be used if specifying
#' one value for \code{d}, \code{n}, and \code{conf.level}.
#' @param w The desired 'half-width' or margin of error of the confidence
#' interval.
#' @param extensive Whether to only return the required sample size, or more
#' extensive results.
#' @param silent Whether to provide \code{FALSE} or suppress (\code{TRUE})
#' warnings.  This is useful because function 'qt', which is used under the
#' hood (see \code{\link{qt}} for more information), warns that 'full precision
#' may not have been achieved' when the density of the distribution is very
#' close to zero. This is normally no cause for concern, because with sample
#' sizes this big, small deviations have little impact.
#' @return \code{dCohensd} (or \code{dd}) gives the density, \code{pCohensd}
#' (or \code{pd}) gives the distribution function, \code{qCohensd} (or
#' \code{qd}) gives the quantile function, and \code{rCohensd} (or \code{rd})
#' generates random deviates.
#' 
#' \code{pdExtreme} returns the probability (or probabilities) of finding a
#' Cohen's \emph{d} equal to or more extreme than the specified value(s).
#' 
#' \code{pdMild} returns the probability (or probabilities) of finding a
#' Cohen's \emph{d} equal to or \emph{less} extreme than the specified
#' value(s).
#' 
#' \code{pdInterval} returns the probability of finding a Cohen's \emph{d} that
#' lies in between the two specified values of Cohen's \emph{d}.
#' 
#' \code{cohensdCI} provides the confidence interval(s) for a given Cohen's
#' \emph{d} value.
#' 
#' \code{pwr.cohensdCI} provides the sample size required to obtain a
#' confidence interval for Cohen's \emph{d} with a desired width.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{convert.d.to.t}}, \code{\link{convert.t.to.d}},
#' \code{\link{dt}}, \code{\link{pt}}, \code{\link{qt}}, \code{\link{rt}}
#' @references
#' 
#' Peters, G. J. Y. & Crutzen, R. (2017) Knowing exactly how effective an
#' intervention, treatment, or manipulation is and ensuring that a study
#' replicates: accuracy in parameter estimation as a partial solution to the
#' replication crisis. http://dx.doi.org/
#' 
#' Maxwell, S. E., Kelley, K., & Rausch, J. R. (2008). Sample size planning for
#' statistical power and accuracy in parameter estimation. Annual Review of
#' Psychology, 59, 537-63.
#' https://doi.org/10.1146/annurev.psych.59.103006.093735
#' 
#' Cumming, G. (2013). The New Statistics: Why and How. Psychological Science,
#' (November). https://doi.org/10.1177/0956797613504966
#' @keywords univar
#' @examples
#' 
#' ### Confidence interval for Cohen's d of .5
#' ### from a sample of 200 participants, also
#' ### showing this visually: this clearly shows
#' ### how wildly our Cohen's d value can vary
#' ### from sample to sample.
#' cohensdCI(.5, n=200, plot=TRUE);
#' 
#' ### How many participants would we need if we
#' ### would want a more accurate estimate, say
#' ### with a maximum confidence interval width
#' ### of .2?
#' pwr.cohensdCI(.5, w=.1);
#' 
#' ### Show that 'sampling distribution':
#' cohensdCI(.5,
#'           n=pwr.cohensdCI(.5, w=.1),
#'           plot=TRUE);
#' 
#' ### Generate 10 random Cohen's d values
#' rCohensd(10, 20, populationD = .5);
#' 
#' ### Probability of findings a Cohen's d smaller than
#' ### .5 if it's 0 in the population (i.e. under the
#' ### null hypothesis)
#' pCohensd(.5, 64);
#' 
#' ### Probability of findings a Cohen's d larger than
#' ### .5 if it's 0 in the population (i.e. under the
#' ### null hypothesis)
#' 1 - pCohensd(.5, 64);
#' 
#' ### Probability of findings a Cohen's d more extreme
#' ### than .5 if it's 0 in the population (i.e. under
#' ### the null hypothesis)
#' pdExtreme(.5, 64);
#' 
#' ### Probability of findings a Cohen's d more extreme
#' ### than .5 if it's 0.2 in the population.
#' pdExtreme(.5, 64, populationD = .2);
#' 
#' @export dCohensd
dCohensd <- dd <- function(x, df=NULL,
                           populationD = 0,
                           n=NULL,
                           n1=NULL,
                           n2=NULL,
                           silent=FALSE) {
  if (!is.null(n1) && !is.null(n2)) {
    ### We have both n's; we don't need to do anything
  } else if (!is.null(n)) {
    ### We have the total n
    n1 <- floor(n/2);
    n2 <- n-n1;
    if (!silent) {
      cat0("Using total sample size (n=", n, "); ",
           "assuming equal group sizes (n1=", n1,
           ", n2=", n2, "). Specify n1 and n2 ",
           "(instead of n) to override this.\n");
    }
  } else if (!is.null(df)) {
    ### We have the degrees of freedom
    n <- df + 2;
    n1 <- floor(n/2);
    n2 <- n-n1;
    if (!silent) {
      cat0("Using degrees of freedom (df=", df,
           ", n=", n, "); ",
           "assuming equal group sizes (n1=", n1,
           ", n2=", n2, "). Specify n1 and n2 ",
           "(instead of n) to override this.\n");
    }
  } else {
    stop("I need at least the degrees of freedom (df), ",
         "the total sample size (n; df+2), or the ",
         "sample size in each group (n1 & n2).\n");
  }
  multiplier <- sqrt(n1*n2/(n1+n2));
  ### Return density for given Cohen's d
  return(multiplier*
           dt(x=convert.d.to.t(x, n1=n1, n2=n2),
              df=(n1+n2-2),
              ncp=convert.d.to.t(populationD, n1=n1, n2=n2)));
}

pCohensd <- pd <- function(q, df, populationD = 0, lower.tail=TRUE) {
  ### Return p-value for given Cohen's d
  return(pt(convert.d.to.t(q, df=df), df,
            ncp=convert.d.to.t(populationD, df=df),
            lower.tail=lower.tail));
}

qCohensd <- qd <- function(p, df, populationD = 0, lower.tail=TRUE) {
  ### Return Cohen's d for given p-value
  return(convert.t.to.d(qt(p, df,
                           ncp=convert.d.to.t(d=populationD,
                                              df=df),
                           lower.tail=lower.tail), df + 2));
}

rCohensd <- rd <- function(n, df, populationD = 0) {
  ### Return random Cohen's d value(s)
  return(convert.t.to.d(rt(n, df=df,
                           ncp=convert.d.to.t(d=populationD,
                                              df=df)),
                        df=df));
}

pdInterval <- function(ds, n, populationD = 0) {
  return(pd(max(ds), df=n - 2, populationD=populationD) -
           pd(min(ds), df=n - 2, populationD=populationD));
}

pdExtreme <- function(d, n, populationD = 0) {
  return(2 * pd(d, n - 2, populationD=populationD,
                lower.tail = (d <= populationD)));
}

pdMild <- function(d, n, populationD = 0) {
  return(1 - pdExtreme(d, n, populationD=populationD));
}
