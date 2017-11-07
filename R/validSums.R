validSums <- function(...,
                       requiredValidValues = .8,
                       returnIfInvalid = NA,
                       silent = FALSE) {
  dat <- list(...);
  if ((length(dat) == 1) && is.data.frame(dat[[1]])) {
    dat <- dat[[1]];
  } else if (length(unique(lapply(dat, length)))==1) {
    dat <- as.data.frame(dat);
  } else {
    stop("The vectors you provided do not have equal lengths! Either provide a dataframe or vectors of the same length");
  }
  if (!is.numeric(requiredValidValues)) {
    stop("Argument 'requiredValidValues' must be numeric, but is ", class(requiredValidValues), ".");
  }
  if (requiredValidValues < 1) {
    requiredValidValuesPercentages <- requiredValidValues;
    requiredValidValues <- ceiling(requiredValidValues * ncol(dat));
    if (!silent) {
      cat0("Argument 'requiredValidValues' was set to a proportion (",
           requiredValidValuesPercentages, "), so only computing a sum for cases ",
           "where that proportion of variables (i.e. ",
           100 * requiredValidValuesPercentages,
           "%, or ", requiredValidValues, " variables) have valid values.\n");
    }
  }
  nrOfValidValues <- rowSums(!is.na(dat)) >= requiredValidValues;
  return(ifelse(nrOfValidValues, rowSums(dat), returnIfInvalid));
}
