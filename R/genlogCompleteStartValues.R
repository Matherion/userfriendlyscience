genlogCompleteStartValues <- function(data,
                                      timeVar = 1,
                                      yVar = 2,
                                      phaseVar = NULL,
                                      baselineMeasurements = NULL, ### Was nA
                                      yRange = NULL,
                                      startX = NULL,               ### Was Xs
                                      startBase = NULL,            ### Was ABs
                                      startTop = NULL,             ### Was ATs
                                      startGrowthRate = NULL,      ### Was Bs
                                      startV = 1,
                                      changeInitiationBounds = NULL,
                                      growthRateBounds = c(-2, 2),
                                      baseMargin = c(0, 3),
                                      topMargin = c(-3, 0),
                                      baseBounds = NULL,
                                      topBounds = NULL,
                                      vBounds = c(1, 1),
                                      returnFullObject = FALSE) {

  res  <- list(input = as.list(environment()),
               intermediate = list(),
               output = list());
  
  ### Store names for easy access later on
  res$intermediate$yVarName <- yVar <-
    ifelse(is.numeric(yVar),
           names(data)[yVar],
           yVar);
  res$intermediate$timeVarName <- timeVar <-
    ifelse(is.numeric(timeVar),
           names(data)[timeVar],
           timeVar);
  res$intermediate$phaseVarName <- phaseVar <-
    ifelse(is.null(phaseVar),
           "none",
           ifelse(is.numeric(timeVar),
                  names(data)[phaseVar],
                  phaseVar));
  
  if (phaseVar=="none") {
    data <- data[, c(timeVar, yVar)];
  } else {
    data <- data[, c(timeVar, yVar, phaseVar)];
  }
  
  ### Remove cases with missing values
  res$intermediate$originalCases <- nrow(data);
  data <- data[complete.cases(data), ];
  res$intermediate$usedCases <- nrow(data);
  res$intermediate$omittedCases <-
    res$intermediate$originalCases - res$intermediate$usedCases;

  ### If the time variable is actually provided as time instead of as
  ### indices/ranks, convert to numeric first.
  if (!is.numeric(data[, timeVar])) {
    if (any(class(data[, timeVar]) %in% c('Date', 'POSIXct', 'POSIXt', 'POSIXt'))) {
      res$intermediate$day0 <- min(data[, timeVar], na.rm=TRUE);
      res$intermediate$day0.formatted <- as.character(res$intermediate$day0);
      data[, timeVar] <- as.numeric(data[, timeVar]) - as.numeric(res$intermediate$day0);
    } else {
      stop("The timeVar variable does not have a class I can work with (numeric or date): instead it has class ",
           vecTxtQ(class(data[, timeVar])), ".");
    }
  }
  
  ### Number of measurements in pre-intervention phase
  if (is.null(baselineMeasurements) && is.null(phaseVar)) {
    stop("Provide number of measurements in pre-intervention phase, either by ",
         "specifying the variable indicating the phase in 'phaseVar', or by ",
         "specifying the number of baseline measurements in 'baselineMeasurements'.");
  } else {
    res$intermediate$baselineMeasurements <-
      baselineMeasurements <-
      ifelse(is.null(baselineMeasurements),
             sum(data[, phaseVar] == min(data[, phaseVar])),
             baselineMeasurements);
  }
  
  ### Starting values for starting to estimate the sigmoid parameters
  res$intermediate$startX <-
    startX <-
    ifelse(is.null(startX),
           data[order(data[, timeVar],
                      decreasing=FALSE)[baselineMeasurements+4],
                timeVar],
           startX);

  res$intermediate$startGrowthRate <-
    startGrowthRate <-
    ifelse(is.null(startGrowthRate),
           0,
           startGrowthRate);
  res$intermediate$startBase <-
    startBase <-
    ifelse(is.null(startBase),
           min(data[, yVar]),
           startBase);
  res$intermediate$startTop <-
    startTop <-
    ifelse(is.null(startTop),
           max(data[, yVar]),
           startTop);
  
  ######################################################################
  ### Prepare starting values and parameter bounds
  ######################################################################
  
  ### Get specified yRange or derive range from observations
  res$intermediate$yRange <-
    yRange <- ifelseObj(is.null(yRange),
                        range(data[, yVar]),
                        yRange);
  
  ### Same for the initiation of the change
  res$intermediate$changeInitiationBounds <-
    changeInitiationBounds <- ifelseObj(is.null(changeInitiationBounds),
                                        c(### Last-but-two baseline measurement
                                          data[order(data[, timeVar],
                                                     decreasing=FALSE)[baselineMeasurements-1],
                                               timeVar],
                                          ### Fifth last element
                                          data[order(data[, timeVar],
                                                     decreasing=TRUE)[5],
                                               timeVar]),
                                        changeInitiationBounds);
  
  ### And the base (floor) and top (ceiling) bounds/constraints
  res$intermediate$baseBounds <-
    baseBounds <- ifelseObj(is.null(baseBounds),
                            c(min(yRange) + baseMargin[1],
                              min(yRange) + baseMargin[2]),
                            baseBounds);
  res$intermediate$topBounds <-
    topBounds <- ifelseObj(is.null(topBounds),
                           c(max(yRange) + topMargin[1],
                             max(yRange) + topMargin[2]),
                           topBounds);
  
  ### Store in lists for convenient passing to optimization function
  res$output$startingValues <-
    startingValues <- c(x0 = startX,
                        B = startGrowthRate,
                        Ab = startBase,
                        At = startTop,
                        v = startV);
  
  res$output$lowerBounds <-
    lowerBounds <- c(init = changeInitiationBounds[1],
                     grow = growthRateBounds[1],
                     base = baseBounds[1],
                     top = topBounds[1],
                     v = vBounds[1]);
  
  res$output$upperBounds <-
    upperBounds <- c(init = changeInitiationBounds[2],
                     grow = growthRateBounds[2],
                     base = baseBounds[2],
                     top = topBounds[2],
                     v = vBounds[2]);
  
  if (returnFullObject) {
    return(res);
  } else {
    return(res$output);
  }

}
