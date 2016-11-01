### This function generates a confidence level for a single mean
meanConfInt <- function(vector=NULL, mean=NULL, sd=NULL, n=NULL, se=NULL, conf.level=.95) {
  if (is.null(mean) & is.null(sd) & is.null(n) & is.null(se)) {
    if (is.null(vector)) {
      stop("Please specify either a vector with datapoints, or a mean and then also either sd and n or se!");
    }
    mean <- mean(vector);
    sd <- sd(vector);
    n <- length(vector);
    se <- sd/sqrt(n);
  }
  else if (!is.null(mean) & !is.null(sd) & !is.null(n)) {
    se <- sd/sqrt(n);
  }
  else if (is.null(mean) | is.null(se)) {
    stop("Please specify either a vector with datapoints, or a mean and then also either sd and n or se!");
  }
  
  res <- list();
  res$input <- list(vector=vector, mean=mean, sd=sd, n=n, se=se, conf.level=conf.level);
  res$intermediate <- list(alpha = 1-conf.level);
  res$intermediate$t.bound.lo <- qt(res$intermediate$alpha/2, df=n-1);
  res$intermediate$t.bound.hi <- qt(1-res$intermediate$alpha/2, df=n-1);
  ci.lo <- mean + res$intermediate$t.bound.lo * se;
  ci.hi <- mean + res$intermediate$t.bound.hi * se;
  res$output <- list(ci = matrix(c(ci.lo, ci.hi), ncol=2));
  colnames(res$output$ci) <- c('ci.lo', 'ci.hi');
  rownames(res$output$ci) <- sprintf("(mean=%.2f)", mean);
  class(res) <- 'meanConfInt';
  return(res);
}

print.meanConfInt <- function(x, digits=2, ...) {
  print(round(x$output$ci, digits=digits), ...);
}
