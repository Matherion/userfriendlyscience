pwr.omegasq <- function(k = NULL, n = NULL,
                        omegasq = NULL,
                        sig.level=.05,
                        power = NULL,
                        digits=4) {
  
  res <- as.list(environment());
  res$es <- 'omegasq';
  res$note <- "n is number in each group";
  res$method <- paste("Balanced one-way analysis of variance",
                      "power calculation");
  class(res) <- "power.htest.usf";

  ### Check assumptions
  if (sum(sapply(list(k, n, omegasq, power, sig.level), is.null)) != 1) {
    stop("Exactly one of 'k', 'n', 'omegasq', 'power', and 'sig.level' must be NULL ",
         "so that I can compute that remaining number.")
  };
  if (!is.null(omegasq) && any(0 > omegasq | omegasq > 1)) {
    stop("Effect size ('omegasq') must be between 0 and 1.");
  }
  if (k < 2) {
    stop("Number of groups ('k') must be at least 2.");
  }
  if (!is.null(n) && n < 2) {
    stop("Number of observations in each group ('n') must be at least 2.")
  }
  if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > sig.level | sig.level > 1)) {
    stop("The significance level ('sig.level') must be numeric and between 0 and 1.");
  }
  if (!is.null(power) && !is.numeric(power) || any(0 > power | power > 1)) {
    stop("The power ('power') must be numeric and between 0 and 1.")
  }

  if (!is.null(omegasq)) {
    ### Compute Cohen's f, which is used by the pwr Anova power function
    res$cohensf <- convert.omegasq.to.cohensf(omegasq);
  }

  if (is.null(k)) {
    res$k <- pwr.anova.test(k=k, n=n,
                            f=res$cohensf,
                            sig.level=sig.level,
                            power=power)$k;
  }

  if (is.null(n)) {
    res$n <- pwr.anova.test(k=k, n=n,
                            f=res$cohensf,
                            sig.level=sig.level,
                            power=power)$n;
  }

  if (is.null(omegasq)) {
    res$omegasq <- convert.cohensf.to.omegasq(pwr.anova.test(k=k, n=n,
                                                             f=NULL,
                                                             sig.level=sig.level,
                                                             power=power)$f);
  }

  if (is.null(sig.level)) {
    res$sig.level <- pwr.anova.test(k=k, n=n,
                                    f=res$cohensf,
                                    sig.level=sig.level,
                                    power=power)$sig.level;
  }

  if (is.null(power)) {
    res$power <- pwr.anova.test(k=k, n=n,
                                f=res$cohensf,
                                sig.level=sig.level,
                                power=power)$power;
  }
  
  return(res);
  
}

print.power.htest.usf <- function(x, digits=x$digits, ...) {
  cat0("\n");
  cat0("     ", x$method, "\n");
  cat0("\n");
  cat0(repStr(14), "k = ", round(x$k, digits), "\n");
  cat0(repStr(14), "n = ", round(x$n, digits), "\n");
  cat0(repStr(15 - nchar(x$es)), x$es, " = ", round(x[[x$es]], digits), "\n");
  cat0(repStr(6), "sig.level = ", round(x$sig.level, digits), "\n");
  cat0(repStr(10), "power = ", round(x$power, digits), "\n");
  cat0("\n");
  if(!is.null(x$note)) {
    cat0("NOTE: ", x$note, "\n\n");
  }
}
