convert.d.to.nnc <- function(d, cer, r = 1, eventDesirable=TRUE) {

  ### Based on http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0019070
  ### Consistent with http://rpsychologist.com/d3/cohend/

  d <- convert.r.to.d(convert.d.to.r(d) * r);

  if (is.null(cer)) {
    if (eventDesirable) {
      return(1 / (2 * pnorm(d / sqrt(2)) - 1));
    } else {
      cat0("Not implemented yet!");
    }
  } else {
    if (eventDesirable) {
      eer <- convert.d.to.eer(d, cer);
      nnc <- 1 / (eer - cer);
    } else {
      eer <- convert.d.to.eer(d, cer);
      nnc <- 1 / (cer - eer);
    }
  }
  attr(nnc, 'eer') <- eer;
  return(nnc);
}
