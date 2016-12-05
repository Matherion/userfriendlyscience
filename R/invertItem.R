### To invert mirrored items
invertItem <- function(item, fullRange=NULL, ignorePreviousInversion = FALSE) {
  ### Check whether this was already inverted
  if (!is.null(attr(item, "inverted"))) {
    if ((attr(item, "inverted") == TRUE) & !(ignorePreviousInversion)) {
      warning("Vector '", substitute(deparse(item)),
              "' has already been inverted! ",
              "Set ignorePreviousInversion to TRUE to override this ",
              "check and invert the vector anyway.");
    }
  }
  
  ### Not inverted yet (or ignorePreviousInversion set to TRUE)
  if (is.numeric(item)) {
    if (is.null(fullRange)) {
      fullRange <- range(item, na.rm=TRUE);
    }
    else {
      fullRange <- range(fullRange);
    }
    res <- sum(fullRange) - item;
  }
  else {
    stop("Provide a numeric vector!");
  }
  if (is.null(attr(item, "inverted"))) {    
    attr(res, "inverted") <- TRUE;
  } else {
    attr(res, "inverted") <- !(attr(res, "inverted"));
  }
  return(res);
}
