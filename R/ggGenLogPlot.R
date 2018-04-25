ggGenLogPlot <- function(data,
                         timeVar = 1,
                         yVar = 2,
                         phaseVar = NULL,
                         baselineMeasurements = NULL, ### Was nA
                         yRange = NULL,
                         startInflection = NULL,      ### Was Xs
                         startBase = NULL,            ### Was ABs
                         startTop = NULL,             ### Was ATs
                         startGrowthRate = NULL,      ### Was Bs
                         startV = 1,
                         inflectionPointBounds = NULL,
                         growthRateBounds = c(-2, 2),
                         baseMargin = c(0, 3),
                         topMargin = c(-3, 0),
                         baseBounds = NULL,
                         topBounds = NULL,
                         vBounds = c(1, 1),
                         changeDelay = 4,
                         colors = list(bottomBound = viridis(4)[4],
                                       topBound = viridis(40)[37],
                                       curve = viridis(4)[3],
                                       mid = viridis(4)[2],
                                       intervention = viridis(4)[1],
                                       points = "black",
                                       outsideRange = "black"),
                         alphas = list(outsideRange = .2,
                                       bounds = .2,
                                       points = .5,
                                       mid = .2),
                         theme = theme_minimal(),
                         pointSize = 2,
                         lineSize = .5,
                         yBreaks = NULL,
                         initialValuesLineType = "dashed",
                         curveSizeMultiplier = 2,
                         plotLabs = NULL,
                         outputFile = NULL,
                         outputWidth = 16,
                         outputHeight = 16,
                         ggsaveParams = list(units='cm',
                                             dpi=300,
                                             type="cairo")) {
  
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
      ### Compute number of days since first measurement
      data[, timeVar] <- (as.numeric(data[, timeVar]) - as.numeric(res$intermediate$day0)) / 86400;
    } else {
      stop("The timeVar variable does not have a class I can work with (numeric or date): instead it has class ",
           vecTxtQ(class(data[, timeVar])), ".");
    }
  }
  
  ### Get starting values if they're not provided yet
  res$intermediate$completeStartValues <-
    genlogCompleteStartValues(data,
                              timeVar = timeVar,
                              yVar = yVar,
                              phaseVar = phaseVar,
                              baselineMeasurements = baselineMeasurements,
                              yRange = yRange,
                              startInflection = startInflection,
                              startBase = startBase,
                              startTop = startTop,
                              startGrowthRate = startGrowthRate,
                              startV = startV,
                              changeDelay = changeDelay,
                              inflectionPointBounds = inflectionPointBounds,
                              growthRateBounds = growthRateBounds,
                              baseMargin = baseMargin,
                              topMargin = topMargin,
                              baseBounds = baseBounds,
                              topBounds = topBounds,
                              vBounds = vBounds,
                              returnFullObject = TRUE);

  ### Get values for convenient use later on
  startInflection <-
    res$intermediate$completeStartValues$intermediate$startInflection;
  baselineMeasurements <-
    res$intermediate$completeStartValues$intermediate$baselineMeasurements;
  yRange <-
    res$intermediate$completeStartValues$intermediate$yRange;
  baseBounds <-
    res$intermediate$completeStartValues$intermediate$baseBounds;
  topBounds <-
    res$intermediate$completeStartValues$intermediate$topBounds;
  inflectionPointBounds <-
    res$intermediate$completeStartValues$intermediate$inflectionPointBounds;
  startBase <-
    res$intermediate$completeStartValues$intermediate$startBase;
  startTop <-
    res$intermediate$completeStartValues$intermediate$startTop;
  
  interventionMoment <- mean(data[order(data[, timeVar],
                                        decreasing=FALSE)[c(baselineMeasurements,
                                                            baselineMeasurements+1)], timeVar]);
  
  if (!is.null(res$intermediate$day0) |
      any(class(data[, timeVar]) %in% c('Date', 'POSIXct', 'POSIXt', 'POSIXt'))) {
    data[, timeVar] <-
      as.POSIXct(86400*data[, timeVar],
                 origin = res$intermediate$day0);
    startInflection <- as.POSIXct(86400*startInflection,
                                  origin = res$intermediate$day0);
    interventionMoment <- as.POSIXct(86400*interventionMoment,
                                     origin = res$intermediate$day0);
    inflectionPointBounds <- as.POSIXct(86400*inflectionPointBounds,
                                        origin = res$intermediate$day0);
  }
  
  if (is.null(plotLabs)) {
    plotLabs <- list(x = ifelse(is.null(res$intermediate$day0.formatted),
                                "Measurements",
                                "Date"),
                     #paste0("Days since ", res$intermediate$day0.formatted)),
                     y = yVar);
  }

  if (is.null(yRange)) {
    yRange <- range(data[, yVar], na.rm=TRUE);
  }
  
  if (is.null(yBreaks)) {
    yBreaks <- pretty(data[, yVar],
                      n=(floor(max(yRange) - min(yRange))));
  } else {
    yBreaks <- seq(from = floor(min(yRange)),
                   to = ceiling(max(yRange)),
                   by = yBreaks)
  }
  
  res$output$plot <-
    ggplot(data, aes_string(x=timeVar, y=yVar)) +
    
    ### Rectangles showing valid values - we pass one line of
    ### data to make sure the rectangle is only drawn once.
    geom_rect(data=data[1, ],
              xmin=-Inf, xmax=Inf,
              ymin=max(yRange), ymax=Inf,
              fill=colors$outsideRange,
              color=NA,
              alpha=alphas$outsideRange) +
    geom_rect(data=data[1, ],
              xmin=-Inf, xmax=Inf,
              ymin=min(yRange), ymax=-Inf,
              fill=colors$outsideRange,
              color=NA,
              alpha=alphas$outsideRange) +
    
    ### Constraints and boundary for floor and ceiling
    geom_rect(data=data[1, ],
              xmin=-Inf, xmax=Inf,
              ymin=min(baseBounds), ymax=max(baseBounds),
              fill=colors$bottomBound,
              color=NA,
              alpha=alphas$bounds) +
    geom_rect(data=data[1, ],
              xmin=-Inf, xmax=Inf,
              ymin=min(topBounds), ymax=max(topBounds),
              fill=colors$topBound,
              color=NA,
              alpha=alphas$bounds) +

    ### Constraints and boundary for change initiation
    geom_rect(data=data[1, ],
              ymin=-Inf, ymax=Inf,
              xmin=min(inflectionPointBounds), xmax=max(inflectionPointBounds),
              fill=colors$mid,
              color=NA,
              alpha=alphas$mid) +
    
    ### Specified intervention moment
    geom_vline(xintercept=startInflection,
               color=colors$mid,
               size=lineSize,
               linetype=initialValuesLineType) +
    
    ### Starting points for floor and ceiling
    geom_hline(yintercept=startTop,
               color = colors$topBound,
               size=lineSize,
               linetype=initialValuesLineType) +
    geom_hline(yintercept=startBase,
               color = colors$bottomBound,
               size=lineSize,
               linetype=initialValuesLineType) +
    
    ### Specified intervention moment
    geom_vline(xintercept=interventionMoment,
               color=colors$intervention,
               size=lineSize) +
    
    ### Data
    geom_point(size = pointSize,
               alpha = alphas$points,
               color = colors$points) +
    theme +
    do.call(labs, plotLabs) +
    coord_cartesian(ylim=yRange) +
    scale_y_continuous(breaks=yBreaks);
  
  if (!is.null(res$intermediate$day0)) {
    res$output$plot <-
      res$output$plot + scale_x_datetime(date_breaks="2 months",
                                         date_labels="%m-%Y");
  }
  
  if (!is.null(outputFile)) {
    ggsaveParameters <- c(list(filename = outputFile,
                               plot = plot,
                               width = outputWidth,
                               height = outputHeight),
                          ggsaveParams);
    do.call(ggsave, ggsaveParameters);
  }
  
  return(res$output$plot);
  
}
