ggBoxplot <- function(dat, y, x = NULL, ...) {
  if (!(y %in% names(dat))) {
    stop("Argument 'y' should be a text string specifying a variable in the ",
         "dataframe specified by 'dat', but '", y, "' isn't among ",
         "names(dat). Please check your spelling, and remember that R is ",
         "case sensitive: it matters whether you use capitals or not!");
  }
  if (is.null(x)) {
    return(ggplot(dat, aes_string(y=y)) + geom_boxplot(aes(x=factor(y))) +
             xlab("") +
             theme_bw());
  } else {
    if (!(x %in% names(dat))) {
      stop("Argument 'x' should be a text string specifying a variable in the ",
           "dataframe specified by 'dat', but '", x, "' isn't among ",
           "names(dat). Please check your spelling, and remember that R is ",
           "case sensitive: it matters whether you use capitals or not!");
    } else if (!is.factor(dat[, x])) {
      dat[, x] <- factor(dat[, x]);
    }
    return(ggplot(dat, aes_string(y=y, x=x)) + geom_boxplot() +
             theme_bw());
  }
}
