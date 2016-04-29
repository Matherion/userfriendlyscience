### This function generates a confidence level for a standard deviation
### http://www.graphpad.com/guides/prism/6/statistics/index.htm?stat_confidence_interval_of_a_stand.htm
### https://www.wolframalpha.com/input/?i=confidence+interval+for+a+standard+deviation&lk=3
sdConfInt <- function(vector=NULL, sd=NULL, n=NULL, conf.level=.95) {
  if (is.null(sd) & is.null(n)) {
    if (is.null(vector)) {
      stop("Please specify either vector, or sd and n!");
    }
    sd <- sd(vector);
    n <- length(vector);
  }
  res <- list();
  res$input <- list(vector=vector, sd=sd, n=n, conf.level=conf.level);
  res$intermediate <- list(alpha = 1-conf.level);
  res$intermediate$chisq.bound.lo <- qchisq(((1-res$intermediate$alpha)/2), n-1);
  res$intermediate$chisq.bound.hi <- qchisq(res$intermediate$alpha/2, n-1);
  ci.lo <- sqrt(((n-1)*sd^2)/res$intermediate$chisq.bound.lo);
  ci.hi <- sqrt(((n-1)*sd^2)/res$intermediate$chisq.bound.hi);
  res$output <- list(ci = c(ci.lo, ci.hi));
  class(res) <- 'sdConfInt';
  return(res);
}

print.sdConfInt <- function(x, digits=2, ...) {
  print(x$output$ci, digits=digits, ...);
}
