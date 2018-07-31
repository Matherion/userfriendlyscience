#' Power analysis for establishing a prevalence
#' 
#' This function can be used to establish how many participants are required to
#' establish a prevalence rate with a given margin of error.
#' 
#' Note that when uncertain as to the expected prevalence, it's better to
#' assume a prevalence closer to 50\%. Prevalences closer to 0\% or 100\% are
#' easier to detect and therefore have more power.
#' 
#' @param expectedPrevalence The expected prevalence.
#' @param marginOfError The desired precision.
#' @param conf.level The confidence of the confidence interval.
#' @return The required number of participants.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{convert.percentage.to.se}}
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ### Required participants for detecting a prevalence of 10%
#' ### with a 95% confidence interval of 10% wide:
#' prevalencePower(.1);
#' 
#' ### Required participants for detecting a prevalence of 10%
#' ### with a 95% confidence interval of 4% wide:
#' prevalencePower(.1, .02);
#' 
#' ### Required participants for detecting a prevalence of 60%
#' ### with a 95% confidence interval of 10% wide:
#' prevalencePower(.6);
#' 
#' @export prevalencePower
prevalencePower <- function(expectedPrevalence, marginOfError = .05, conf.level = .95) {
  ### From http://www.r-tutor.com/elementary-statistics/interval-estimation/sampling-size-population-proportion
  ### and http://elearning.winona.edu/projects/N701/Powerpoints/TestingSingleProp.ppt
  qnorm(1-((1-conf.level)/2)) ^2 * expectedPrevalence * (1-expectedPrevalence) / marginOfError^2;
}
