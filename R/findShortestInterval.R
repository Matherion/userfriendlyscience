### Function to find minimum shortest interval in numeric vector
findShortestInterval <- function(x) {
  if (!is.numeric(x))
    stop("This function only accepts numeric vectors as input.");
  if (length(x) == 1)
    return(x);
  x <- sort(x);
  i <- 1;
  res <- abs(x[i] - x[i+1]);
  while (i < length(x)) {
    if (res > abs(x[i] - x[i+1])) res <- abs(x[i] - x[i+1]);
    i <- i + 1;
  }
  return(res);
}
