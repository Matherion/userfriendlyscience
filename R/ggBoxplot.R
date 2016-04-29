ggBoxplot <- function(dat, y = NULL, x = NULL,
                      theme = theme_bw(), ...) {
  if (is.null(x) && is.null(y)) {
    if (is.vector(dat, mode="numeric")) {
      varname <- deparse(substitute(dat));
      tmpDf <- data.frame(dat);
      names(tmpDf) <- varname;
      return(ggplot(tmpDf, aes_string(y=varname)) +
               geom_boxplot(aes(x=factor(varname))) +
               xlab("") + theme_bw() +
               theme(axis.text.x = element_blank(),
                     axis.ticks.x = element_blank()));
    } else {
      stop("If both arguments 'x' and 'y' are NULL, the first argument, 'dat, ",
           "should be a vector, but instead, it has class '", class(dat),
           "'.");
    }
    
    if (!(y %in% names(dat))) {
      stop("Argument 'y' should be a text string specifying a variable in the ",
           "dataframe specified by 'dat', but '", y, "' isn't among ",
           "names(dat). Please check your spelling, and remember that R is ",
           "case sensitive: it matters whether you use capitals or not!");
    }
    
    if (is.null(x)) {
      return(ggplot(dat, aes_string(y=y)) + geom_boxplot(aes(x=factor(y))) +
               xlab("") + theme +
               theme(axis.text.x = element_blank(),
                     axis.ticks.x = element_blank()));
    } else {
      if (!(x %in% names(dat))) {
        stop("Argument 'x' should be a text string specifying a variable in the ",
             "dataframe specified by 'dat', but '", x, "' isn't among ",
             "names(dat). Please check your spelling, and remember that R is ",
             "case sensitive: it matters whether you use capitals or not!");
      } else if (!is.factor(dat[, x])) {
        dat[, x] <- factor(dat[, x]);
      }
      return(ggplot(dat, aes_string(y=y, x=x)) + geom_boxplot() + theme);
    }
  }
}
