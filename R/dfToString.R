dfToString <- function(df, width = 300, ...) {
  ### Store width
  prevWidth <- getOption('width');
  options(width = width);
  res <- capture.output(print(df, ...));
  options(width = prevWidth);
  return(res);
}