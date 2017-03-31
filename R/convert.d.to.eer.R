convert.d.to.eer <- function(d, cer) {
  return(pnorm((qnorm(cer) + d)));
}
