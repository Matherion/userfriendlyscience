genlog <- function(data,
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
                   colors = list(bounds = viridis(4)[4],
                                 curve = viridis(4)[3],
                                 mid = viridis(4)[2],
                                 intervention = viridis(4)[1],
                                 points = "black"),
                   theme = theme_minimal(),
                   pointSize = 2,
                   pointAlpha = .5,
                   lineSize = .5,
                   curveSizeMultiplier = 2,
                   showPlot = TRUE,
                   outputFile = NULL,
                   outputWidth = 16,
                   outputHeight = 16,
                   ggsaveParams = list(units='cm',
                                       dpi=300,
                                       type="cairo")) {
  
  result  <- list(input = as.list(environment()),
                  intermediate = list(),
                  output = list());
  
  ### Store names for easy access later on
  result$intermediate$yVarName <-
    ifelse(is.numeric(yVar),
           names(data)[yVar],
           yVar);
  result$intermediate$timeVarName <-
    ifelse(is.numeric(timeVar),
           names(data)[timeVar],
           timeVar);
  result$intermediate$phaseVarName <-
    ifelse(is.null(phaseVar),
           "none",
           ifelse(is.numeric(timeVar),
                  names(data)[phaseVar],
                  phaseVar));

  ### The definition of the generalized logistic function
  result$intermediate$GLF <- GLF <-
    paste0(yVar, "~ Ab + (At - Ab)/ (1 + exp(-B*(", timeVar, " - x0))) ^ (1/v)");
  ### "y ~ Ab + (At - Ab)/ (1 + exp(-B*(x-x0)))**(1/v)";
  
  ### If the time variable is actually provided as time instead of as
  ### indices/ranks, convert to numeric first.
  if (!is.numeric(dat[, timeVar])) {
    if (class(dat[, timeVar]) %in% c('Date', 'POSIXct', 'POSIXt', 'POSIXt')) {
      res$intermediate$day0 <- min(dat[, timeVar]);
      res$intermediate$day0.formatted <- as.character(res$intermediate$day0);
      dat[, timeVar] <- as.numeric(dat[, timeVar] - min(dat[, timeVar]));
    } else {
      stop("The timeVar variable does not have a class I can work with (numeric or date): instead it has class ",
           vecTxtQ(class(dat[, timeVar])), ".");
    }
  }
  
  ### Number of measurements in pre-intervention phase
  if (is.null(baselineMeasurements) && is.null(phaseVar)) {
    stop("Provide number of measurements in pre-intervention phase, either by ",
         "specifying the variable indicating the phase in 'phaseVar', or by ",
         "specifying the number of baseline measurements in 'baselineMeasurements'.");
  } else {
    result$intermediate$baselineMeasurements <-
      baselineMeasurements <-
      ifelse(is.null(baselineMeasurements),
             sum(data[, phaseVar] == min(data[, phaseVar])),
             baselineMeasurements);
  }

  ### Starting values for starting to estimate the sigmoid parameters
  result$intermediate$startX <-
    startX <-
    ifelse(is.null(startX),
           baselineMeasurements + 4,
           startX);
  result$intermediate$startGrowthRate <-
    startGrowthRate <-
    ifelse(is.null(startGrowthRate),
           0,
           startGrowthRate);
  result$intermediate$startBase <-
    startBase <-
    ifelse(is.null(startBase),
           min(data[, yVar]),
           startBase);
  result$intermediate$startTop <-
    startTop <-
    ifelse(is.null(startTop),
           max(data[, yVar]),
           startTop);
  
  ######################################################################
  ### Prepare starting values and parameter bounds
  ######################################################################
  
  ### Get specified yRange or derive range from observations
  result$intermediate$yRange <-
    yRange <- ifelseObj(is.null(yRange),
                        range(data[, yVar]),
                        yRange);
  
  ### Same for the initiation of the change
  changeInitiationBounds <- ifelseObj(is.null(changeInitiationBounds),
                                      c(baselineMeasurements-1,
                                        max(data[, timeVar])-5),
                                      changeInitiationBounds);
  
  result$intermediate$startingValues <-
    startingValues <- list(x0 = startX,
                           B = startGrowthRate,
                           Ab = startBase,
                           At = startTop,
                           v = startV);
  
  result$intermediate$lowerBounds <-
    lowerBounds <- c(init = changeInitiationBounds[1],
                     grow = growthRateBounds[1],
                     base = ifelse(is.null(baseBounds),
                                   min(yRange) + baseMargin[1],
                                   baseBounds[1]),
                     top = ifelse(is.null(topBounds),
                                  max(yRange) + topMargin[1],
                                  topBounds[1]),
                     v = vBounds[1]);
  
  result$intermediate$upperBounds <-
    upperBounds <- c(init = changeInitiationBounds[2],
                     grow = growthRateBounds[2],
                     base = ifelse(is.null(baseBounds),
                                   min(yRange) + baseMargin[2],
                                   baseBounds[2]),
                     top = ifelse(is.null(topBounds),
                                  max(yRange) + topMargin[2],
                                  topBounds[2]),
                     v = vBounds[2]);
  
  ### Optimizing function
  out <- result$intermediate$nlsLM <-
    nlsLM(GLF,
          data  = data,
          start = startingValues,
          lower = lowerBounds,
          upper = upperBounds);
  
  ### Extract coefficients
  result$output$maxChangeMoment <-
    x0 <- as.numeric(coef(out)[1]);
  result$output$growthRate <-
    B <-  as.numeric(coef(out)[2]);
  result$output$base <-
    Ab <- as.numeric(coef(out)[3]);
  result$output$top <-
    At <- as.numeric(coef(out)[4]);
  result$output$v <-
    v <-  as.numeric(coef(out)[5]);
  
  Dev <- deviance(out);
  SSQtot <- sum((data[, yVar] - mean(data[, yVar])) ^ 2);
  
  result$output$Rsq <-
    Rsq <- (SSQtot - Dev) / SSQtot;
  result$output$ES1 <-
    ES1 <- (At -Ab) / sd(data[, yVar]);
  result$output$ES2 <-
    ES2 <- (At -Ab) / diff(yRange);
  result$output$dat <-
    data.frame(Rsq = Rsq,
               ES1 = ES1,
               ES2 = ES2,
               growthRate = B,
               maxChangeMoment = x0,
               base = Ab,
               top = At);
  
  yfit <- genlogFunction(x = data[, timeVar],
                         x0 = x0,
                         Ab = Ab,
                         At = At,
                         B = B,
                         v = v);

  interventionMoment <- mean(data[c(baselineMeasurements,
                                    baselineMeasurements+1), timeVar]);
  
  result$output$plot <- plot <-
    ggplot(data, aes_string(x=timeVar,y=yVar)) +
    geom_hline(yintercept=Ab,
               colour=colors$bounds,
               size=lineSize) +
    geom_hline(yintercept=At,
               colour=colors$bounds,
               size=lineSize) +
    geom_vline(xintercept=x0,
               colour=colors$mid,
               size=lineSize) +
    geom_vline(xintercept=interventionMoment,
               colour=colors$intervention,
               size=lineSize) +
    geom_point(size = pointSize,
               alpha = pointAlpha,
               color = colors$points) +
    geom_line(data=data.frame(x=data[, timeVar], y=yfit),
              aes_string(x='x', y='y'),
              colour=colors$curve,
              size = lineSize * curveSizeMultiplier) +
    theme +
    labs(x = ifelse(is.null(result$intermediate$day0.formatted),
                    "Measurements",
                    paste0("Days since ", result$intermediate$day0.formatted)),
         y = "Score");
  
  if (!is.null(outputFile)) {
    ggsaveParameters <- c(list(filename = outputFile,
                               plot = plot,
                               width = outputWidth,
                               height = outputHeight),
                          ggsaveParams);
    do.call(ggsave, ggsaveParameters);
  }
  
  class(result) <- "genlog";
  
  return(result);
  
}

print.genlog <- function(x, digits=3, ...) {
  if (x$input$showPlot) {
    grid.newpage();
    grid.draw(x$output$plot);
  }
  cat0("Generalized Logistic Analysis\n\n",
       "Estimated sigmoid association between ",
       x$intermediate$timeVarName, " and ",
       x$intermediate$yVarName, ".\n\n");
  cat0("R squared:         ", round(x$output$Rsq, digits=digits), "\n",
       "Effect Size 1:     ", round(x$output$ES1, digits=digits), "\n",
       "Effect Size 2:     ", round(x$output$ES2, digits=digits), "\n",
       "Growth rate:       ", round(x$output$growthRate, digits=digits), "\n",
       "Change maximal at: ", round(x$output$maxChangeMoment, digits=digits), "\n",
       "Curve base:        ", round(x$output$base, digits=digits), "\n",
       "Curve top:         ", round(x$output$top, digits=digits), "\n");
  #print(x$output$dat, digits=digits, row.names=FALSE);
}
