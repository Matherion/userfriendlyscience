### This function does a lot of stuff to detect influential cases


#' Detecting influential cases in regression analyses
#' 
#' This function combines a number of criteria for determining whether a
#' datapoint is an influential case in a regression analysis. It then sum the
#' criteria to compute an index of influentiality. A list of cases with an
#' index of influentiality of 1 or more is then displayed, after which the
#' regression analysis is repeated without those influantial cases. A
#' scattermatrix is also displayed, showing the density curves of each
#' variable, and in the scattermatrix, points that are colored depending on how
#' influential each case is.
#' 
#' 
#' @param formula The formule of the regression analysis.
#' @param data The data to use for the analysis.
#' @return A \code{regrInfluential} object, which, if printed, shows the
#' influential cases, the regression analyses repeated without those cases, and
#' the scatter matrix.
#' @author Gjalt-Jorn Peters & Marwin Snippe
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' regrInfluential(mpg ~ hp, mtcars);
#' 
#' @export regrInfluential
regrInfluential <- function(formula, data) {
  ### Create object for results and store input
  res <- list(input = as.list(environment()), intermediate = list(), 
              output = list())
  ### Get variables in formula
  res$intermediate$variableNames <- all.vars(formula)
  ### Store temporary dataframe
  res$intermediate$dat <-
    data.frame(na.omit(data[, res$intermediate$variableNames]));
  ### Compute outcomes of influence measures
  res$intermediate$influence.measures <-
    influence.measures(lm(formula, data=res$intermediate$dat));
  ### Add to dataframe
  res$intermediate$dat <- data.frame(res$intermediate$dat,
                                     res$intermediate$influence.measures$infmat,
                                     indexOfInfluentiality = rowSums(res$intermediate$influence.measures$is.inf));
  
  ### Generate scattermatrix showing index fo influentiality
  res$output$plot <-
    ggpairs(data=res$intermediate$dat[, c(res$intermediate$variableNames, 'indexOfInfluentiality')],
            columns=1:length(res$intermediate$variableNames),
            upper='blank',
            lower=list(continuous=function(data, mapping, ...) {
              res <- ggplot(data=data, mapping=mapping) +
                geom_point(position="jitter") +
                scale_colour_gradient(low='green', high='red') +
                theme_bw();
              return(res);
            }),
            mapping=ggplot2::aes_string(colour='indexOfInfluentiality'));
  ### Conduct regression analyses for all levels of indexOfInfluentiality
  ### higher than 0
  res$output$regrObjects <- list();
  for (levelOfInfluentiality in sort(unique(res$intermediate$dat$indexOfInfluentiality), decreasing=TRUE)[
    sort(unique(res$intermediate$dat$indexOfInfluentiality), decreasing=TRUE)>0]) {
    res$output$regrObjects[[levelOfInfluentiality]] <-
      regr(formula=formula,
           data=res$intermediate$dat[res$intermediate$dat$indexOfInfluentiality <= levelOfInfluentiality, ]);
  }
  ### Store dataframe with influential cases
  res$output$dat.diagnostics <-
    res$intermediate$dat[res$intermediate$dat$indexOfInfluentiality > 0, ];
  class(res) <- 'regrInfluential';
  return(res)
}

print.regrInfluential <- function(x, ...) {
  print(x$output$dat.diagnostics);
  print(x$output$plot);
  cat("\nRegression analyses, repeated without influential cases:\n");
  for (currentRegr in sort(1:length(x$output$regrObjects), decreasing=TRUE)) {
    if (!is.null(x$output$regrObjects[[currentRegr]])) {
      cat("\n-- Omitting all cases marked as influential by",
          currentRegr,
          "criteria:\n\n");
      print(x$output$regrObjects[[currentRegr]]);
    }
  }
}
