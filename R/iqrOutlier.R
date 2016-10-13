### Based on JasonAizkalns' answer at
### http://stackoverflow.com/questions/33524669/labeling-outliers-of-boxplots-in-r

iqrOutlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
