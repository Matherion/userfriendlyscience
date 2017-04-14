pwr.cohensdCI <- pwr.confIntd <- function(d, w=.1, conf.level=.95, extensive = FALSE, silent=TRUE) {
  if (length(w) != 1) {
    warning("Multiple widths not supported (yet); only the first one is used!");
    w <- w[1];
  }
  if (length(conf.level) != 1) {
    warning("Multiple confidence levels not supported (yet); only the first one is used!");
    conf.level <- conf.level[1];
  }

  ### From a post at the R-help mailig list by Luke Tierney, see
  ### http://stackoverflow.com/questions/3903157/how-can-i-check-whether-a-function-call-results-in-a-warning
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w));
    invokeRestart("muffleWarning");
  }
  myWarnings <- NULL;

  dSign <- ifelse(d < 0, -1, 1);
  d <- abs(d);
  lowerP <- (1-conf.level) / 2;
  upperP <- 1 - ((1-conf.level) / 2);
  lowerBound <- abs(d) - abs(w);
  upperBound <- abs(d) + abs(w);
  n <- numeric();
  for (di in 1:length(d)) {
    n[di] <- 4;
    withCallingHandlers(while (lowerBound[di] > qCohensd(lowerP, df=n[di]-2, populationD=d[di], lower.tail=TRUE) ||
                               upperBound[di] < qCohensd(upperP, df=n[di]-2, populationD=d[di], lower.tail=FALSE)) {
      n[di] <- n[di] + 100;
    }, warning = wHandler);
    if (n[di] > 100) n[di] <- n[di] - 100;
    withCallingHandlers(while (lowerBound[di] > qCohensd(lowerP, df=n[di]-2, populationD=d[di], lower.tail=TRUE) ||
                               upperBound[di] < qCohensd(upperP, df=n[di]-2, populationD=d[di], lower.tail=FALSE)) {
      n[di] <- n[di] + 10;
    }, warning = wHandler);
    if (n[di] > 10) n[di] <- n[di] - 10;
    withCallingHandlers(while (lowerBound[di] > qCohensd(lowerP, n[di]-2, populationD=d[di], lower.tail=TRUE) ||
                               upperBound[di] < qCohensd(upperP, n[di]-2, populationD=d[di], lower.tail=FALSE)) {
      n[di] <- n[di] + 1;
    }, warning = wHandler);
  }
  if (extensive) {
    df <- n - 2;
    res <- list(n = n,
                requestedLowerBound = dSign * lowerBound,
                requestedUpperBound = dSign * upperBound,
                actualLowerBound = dSign * qCohensd(lowerP, df, populationD=d, lower.tail=TRUE),
                actualUpperBound = dSign * qCohensd(upperP, df, populationD=d, lower.tail=FALSE));
    return(res);
  }
  if ((!silent) && (length(myWarnings) > 0)) {
    precisionWarnings <- grepl("full precision may not have been achieved in 'pnt{final}'",
                               myWarnings, fixed = TRUE);
    if (any(precisionWarnings)) {
      cat0("Function 'qt', which is used under the hood of this function (see ?qt for more information), ",
           "warned that 'full precision may not have been achieved'. ",
           "This is normally no cause for concern, because with sample sizes this big, small deviations ",
           "have little impact, but informing you seemed appropriate nonetheless.\n\n");
    }
    if (!all(precisionWarnings)) {
      cat0("One or more ", ifelse(any(precisionWarnings), "additional", ""),
           " warnings were encountered:\n");
      lapply(myWarnings[!precisionWarnings], function(x) cat0(x$message, "\n"));
      cat("\n");
    }
  }
  return(n);
}

#n <- pwr.cohensdCI(4, extensive=T); print(n); cohensdCI(4, n=n$n);


