#' Helper functions for Numbers Needed for Change
#' 
#' These two functions are used by \code{\link{nnc}} to compute the Numbers
#' Needed for Change.
#' 
#' These two functions are used by \code{\link{nnc}} to compute the Numbers
#' Needed for Change.
#' 
#' @aliases convert.d.to.nnc convert.d.to.eer
#' @param d The value of Cohen's \emph{d}.
#' @param cer The Control Event Rate.
#' @param r The correlation between the determinant and behavior (for mediated
#' Numbers Needed for Change).
#' @param eventDesirable Whether an event is desirable or undesirable.
#' @param eventIfHigher Whether scores above or below the threshold are
#' considered 'an event'.
#' @return The converted value.
#' @author Gjalt-Jorn Peters & Stefan Gruijters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{nnc}}
#' @references Gruijters, S. L. K., & Peters, G.-J. Y. (2017). Introducing the
#' Numbers Needed for Change (NNC): A practical measure of effect size for
#' intervention research.
#' @keywords utilities
#' @examples
#' 
#' convert.d.to.eer(d=.5, cer=.25);
#' convert.d.to.nnc(d=.5, cer=.25);
#' 
#' @export convert.d.to.nnc
convert.d.to.nnc <- function(d, cer, r = 1, eventDesirable=TRUE, eventIfHigher=TRUE) {

  ### Based on http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0019070
  ### Consistent with http://rpsychologist.com/d3/cohend/

  d <- convert.r.to.d(convert.d.to.r(d) * r);

  if (is.null(cer)) {
    # if (eventDesirable) {
    #   return(1 / (2 * pnorm(d / sqrt(2)) - 1));
    # } else {
      cat0("Not implemented yet!");
    # }
  } else {
    eer <- convert.d.to.eer(d, cer, eventDesirable=eventDesirable, eventIfHigher=eventIfHigher);
    if (eventDesirable) {
      nnc <- 1 / (eer - cer);
    } else {
      nnc <- 1 / (cer - eer);
    }
  }
  attr(nnc, 'eer') <- eer;
  return(nnc);
}
