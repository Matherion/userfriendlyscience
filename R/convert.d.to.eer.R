convert.d.to.eer <- function(d, cer, eventDesirable=TRUE, eventIfHigher=TRUE) {
  if (eventIfHigher) {
    return(pnorm((qnorm(cer) + d)));
  } else {
    return(1 - pnorm((qnorm(1-cer) + d)));
  }
}
