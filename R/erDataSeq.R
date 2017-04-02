erDataSeq <- function(er = NULL, threshold = NULL, mean = NULL, sd = NULL,
                      eventIfHigher = TRUE,
                      pRange = c(.000001, .99999), xStep=.01) {
  
  if (is.null(er) && is.null(threshold)) {
    stop("Provide either the control event rate (er; a proportion, ",
         "a number between 0 and 1) or the cut-off value that determines ",
         "an 'event' on the same scale as mean and sd.");
  }

  if (is.null(er)) {
    ### Determine er from threshold
    if (is.null(mean) || is.null(sd)) {
      stop("When I need to derive the er from the threshold value, ",
           "you must also provide me with the mean and the standard ",
           "deviation!");
    }
    er <- convert.threshold.to.er(threshold = threshold,
                                  mean = mean,
                                  sd = sd,
                                  eventIfHigher = eventIfHigher);

  if (is.null(threshold)) {
    if (is.null(mean) && is.null(sd)) {
      mean <- 0;
      sd <- 1;
    } else if (is.null(mean)) {
      stop("If providing an event rate (er) and a standard deviation, you must also provide a mean value!");
    } else if (is.null(sd)) {
      stop("If providing an event rate (er) and a mean value, you must also provide a standard deviation!");
    }
    threshold <- convert.er.to.threshold(er,
                                         mean = mean,
                                         sd = sd,
                                         eventIfHigher = eventIfHigher);
  }

  ### Get range from where to where to generate values
  xRange <- c(qnorm(min(pRange), mean=mean, sd=sd),
              qnorm(max(pRange), mean=mean, sd=sd));

  res <- data.frame(x = seq(from=xRange[1], to=xRange[2], by=xStep));
  res$density <- dnorm(res$x, mean=mean, sd=sd);

  attr(res, 'er') <- er;
  attr(res, 'threshold') <- threshold;
  attr(res, 'mean') <- mean;
  attr(res, 'eventIfHigher') <- eventIfHigher;
  attr(res, 'sd') <- sd;

  class(res) <- c('erDataSeq', class(res));

  return(res);

}
