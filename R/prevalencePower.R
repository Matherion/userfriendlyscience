prevalencePower <- function(expectedPrevalence, marginOfError = .05, conf.level = .95) {
  ### From http://www.r-tutor.com/elementary-statistics/interval-estimation/sampling-size-population-proportion
  ### and http://elearning.winona.edu/projects/N701/Powerpoints/TestingSingleProp.ppt
  qnorm(1-((1-conf.level)/2)) ^2 * expectedPrevalence * (1-expectedPrevalence) / marginOfError^2;
}
