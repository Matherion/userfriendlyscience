### repeat a string a given number of times
repeatStr <- repStr <- function (n = 1, str = " ") {
  if (is.character(n) && is.numeric(str)) {
    ### The input was switched.
    tmp <- n;
    n <- str;
    str <- tmp;
    rm(tmp);
  }
  if (n < 1) {
    return("");
  }
  else if (n == 1) {
    return(str);
  }
  else {
    res <- str;
    for(i in c(1:(n-1))) {
      res <- paste0(res, str);
    }
    return(res);
  }
}
