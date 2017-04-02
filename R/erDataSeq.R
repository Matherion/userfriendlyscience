erDataSeq <- function(er = NULL, erValue = NULL, meanValue = NULL, sd = NULL,
                      eventIfHigher = TRUE,
                      pRange = c(.000001, .99999), xStep=.01) {
  if (is.null(er) && is.null(erValue)) {
    stop("Provide either the control event rate (er; a proportion, ",
         "a number between 0 and 1) or the cut-off value that determines ",
         "an 'event' on the same scale as meanValue and sd.");
  }

  if (is.null(er)) {
    ### Determine er from erValue
    if (is.null(meanValue) || is.null(sd)) {
      stop("When I need to derive the er from the threshold value, ",
           "you must also provide me with the mean and the standard ",
           "deviation!");
    }
    z <- (meanValue - erValue) / sd;
    er <- pnorm(erValue, mean=meanValue, sd=sd);
  } else {
    z <- qnorm(p = er);
  }

  if (eventIfHigher) {
    z <- -1 * z;
    er <- 1 - er;
  }

  if (is.null(erValue)) {
    if (is.null(meanValue) && is.null(sd)) {
      meanValue <- 0;
      sd <- 1;
    } else if (is.null(meanValue)) {
      stop("If providing an event rate (er) and a standard deviation, you must also provide a mean value!");
    } else if (is.null(sd)) {
      stop("If providing an event rate (er) and a mean value, you must also provide a standard deviation!");
    }
    erValue <- meanValue + z * sd;
  }

  if (is.null(meanValue)) {
    if (is.null(erValue)) {
      stop("If not providing a mean, you must provide an erValue!");
    }
    if (is.null(sd)) {
      stop("If not providing a mean, you must provide a standard deviation!");
    }
    meanValue <- erValue - z * sd;
  }

  ### Get range from where to where to generate values
  xRange <- c(qnorm(min(pRange), mean=meanValue, sd=sd),
              qnorm(max(pRange), mean=meanValue, sd=sd));

  res <- data.frame(x = seq(from=xRange[1], to=xRange[2], by=xStep));
  res$density <- dnorm(res$x, mean=meanValue, sd=sd);

  attr(res, 'er') <- er;
  attr(res, 'erValue') <- erValue;
  attr(res, 'meanValue') <- meanValue;
  attr(res, 'eventIfHigher') <- eventIfHigher;
  attr(res, 'sd') <- sd;

  class(res) <- c('erDataSeq', class(res));

  return(res);

}
