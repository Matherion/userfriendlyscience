convert.d.to.eer <- function(d, cer, eventDesirable=TRUE) {
  if (eventDesirable) {
    return(pnorm((d + qnorm(cer))));
  } else {
    res <- pnorm((d - qnorm(cer)));
    cat0("Warning: this is experimental and may not always work! Computed EER = ", round(res, 3), ".\n");
    return(res);
  }
}
