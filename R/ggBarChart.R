#' Bar chart using ggplot
#' 
#' This function provides a simple interface to create a \code{\link{ggplot}}
#' bar chart.
#' 
#' 
#' @param vector The vector to display in the bar chart.
#' @param plotTheme The theme to apply.
#' @param \dots And additional arguments are passed to \code{\link{geom_bar}}.
#' @return A \code{\link{ggplot}} plot is returned.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{geom_bar}}
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ggBarChart(mtcars$cyl);
#' 
#' @export ggBarChart
ggBarChart <- function(vector, plotTheme = theme_bw(), ...) {
  varName <- extractVarName(deparse(substitute(vector)));
  tmpDf <- as.data.frame(na.omit(vector));
  names(tmpDf) <- varName;
  ggplot(tmpDf, aes_string(x=varName)) +
    geom_bar(...) + plotTheme;
}
