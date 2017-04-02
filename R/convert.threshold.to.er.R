convert.threshold.to.er <- function(threshold, mean, sd,
                                    eventIfHigher = TRUE,
                                    pdist = pnorm) {
  return(pdist(threshold, mean=mean, sd=sd, lower.tail=!eventIfHigher));
}

convert.er.to.threshold <- function(er, mean, sd,
                                    eventIfHigher = TRUE,
                                    qdist = qnorm) {
  q <- qdist(er);
  if (eventIfHigher) {
    return(mean - q * sd);
  } else {
    return(mean + q * sd);
  }
}
