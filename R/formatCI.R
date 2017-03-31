formatCI <- function(ci, sep='; ', prefix='[', suffix=']', digits=2, noZero=FALSE) {
  if (noZero) {
    return(paste0(prefix, paste0(noZero(round(ci, digits)), collapse=sep), suffix));
  } else {
    return(paste0(prefix, paste0(round(ci, digits), collapse=sep), suffix));
  }
}
