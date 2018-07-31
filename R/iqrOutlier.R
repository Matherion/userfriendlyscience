### Based on JasonAizkalns' answer at
### http://stackoverflow.com/questions/33524669/labeling-outliers-of-boxplots-in-r



#' Identify outliers according to the IQR criterion
#' 
#' The IQR criterion holds that any value lower than one-and-a-half times the
#' interquartile range below the first quartile, or higher than one-and-a-half
#' times the interquartile range above the third quartile, is an outlier. This
#' function returns a logical vector that identifies those outliers.
#' 
#' 
#' @param x The vector to scan for outliers.
#' @return A logical vector where TRUE identifies outliers.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{IQR}}
#' @keywords univariate
#' @examples
#' 
#' ### One outlier in the miles per gallon
#' iqrOutlier(mtcars$mpg);
#' 
#' @export iqrOutlier
iqrOutlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
