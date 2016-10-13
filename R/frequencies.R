frequencies <- function(..., digits = 1, nsmall=1, transposed=FALSE, round=1,
                    plot=FALSE, plotTheme = theme_bw()) {

  ### Call functions to explore the variables
  res <- lapply(list(...), function(x) {
    rsl <- list();
    rsl$freq <- freq(x, digits=digits, nsmall=nsmall,
                     transposed=transposed, round=round,
                     plot=plot, plotTheme=plotTheme);
    return(rsl);
  });

  ### Get the variable names
  names(res) <- unlist(as.list(substitute(list(...)))[-1]);

  ### Set class for correct printing and return result
  class(res) <- 'frequencies';
  return(res);
}

print.frequencies <- function(x, ...) {
  for (currentName in names(x)) {
    cat0("### Frequencies for '", extractVarName(currentName), "'\n\n");
    print(x[[currentName]]$freq);
    cat("\n");
  }
}

pander.frequencies <- function(x, prefix="###", ...) {
  for (currentName in names(x)) {
    cat0(prefix, " Frequencies for '", extractVarName(currentName), "'\n\n");
    pander(x[[currentName]]$freq);
    cat("\n");
  }
}
