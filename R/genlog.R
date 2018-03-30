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
                   colors = list(bottomBound = viridis(4)[4],
                                 topBound = viridis(40)[37],
                                 curve = viridis(4)[3],
                                 mid = viridis(4)[2],
                                 intervention = viridis(4)[1],
                                 points = "black",
                                 outsideRange = "black"),
                   alphas = list(outsideRange = .2,
                                 bounds = 0,
                                 points = .5,
                                 mid = 0),
                   theme = theme_minimal(),
                   pointSize = 2,
                   lineSize = .5,
                   initialValuesLineType = "blank",
                   curveSizeMultiplier = 2,
                   showPlot = TRUE,
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
  
  ### The definition of the generalized logistic function
  res$intermediate$GLF <- GLF <-
    paste0(yVar, " ~ Ab + (At - Ab)/ (1 + exp(-B*(", timeVar, " - x0))) ^ (1/v)");
  ### "y ~ Ab + (At - Ab)/ (1 + exp(-B*(x-x0)))**(1/v)";

  ### If the time variable is actually provided as time instead of as
  ### indices/ranks, convert to numeric first.
  if (!is.numeric(data[, timeVar])) {
    if (any(class(data[, timeVar]) %in% c('Date', 'POSIXct', 'POSIXt', 'POSIXt'))) {
      res$intermediate$day0 <- min(data[, timeVar], na.rm=TRUE);
      res$intermediate$day0.formatted <- as.character(res$intermediate$day0);
      data[, timeVar] <- as.numeric(data[, timeVar]) - as.numeric(min(data[, timeVar]));
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

  ### Get starting values if they're not provided yet
  res$intermediate$completeStartValues <-
    genlogCompleteStartValues(data,
                              timeVar = timeVar,
                              yVar = yVar,
                              phaseVar = phaseVar,
                              baselineMeasurements = baselineMeasurements,
                              yRange = yRange,
                              startX = startX,
                              startBase = startBase,
                              startTop = startTop,
                              startGrowthRate = startGrowthRate,
                              startV = startV,
                              changeInitiationBounds = changeInitiationBounds,
                              growthRateBounds = growthRateBounds,
                              baseMargin = baseMargin,
                              topMargin = topMargin,
                              baseBounds = baseBounds,
                              topBounds = topBounds,
                              vBounds = vBounds,
                              returnFullObject = TRUE);
  
  #print(res$intermediate$completeStartValues);
  
  ### Merge with object with intermediate values
  res$intermediate <- c(res$intermediate,
                        res$intermediate$completeStartValues$intermediate,
                        res$intermediate$completeStartValues$output);
  
  yRange <- res$intermediate$yRange;

  ### Optimizing function
  tryCatch({
    out <- res$intermediate$nlsLM <-
      nlsLM(GLF,
            data  = data,
            start = res$intermediate$startingValues,
            lower = res$intermediate$lowerBounds,
            upper = res$intermediate$upperBounds);
  }, error = function(e) {
    if (!is.null(res$intermediate$day0)) {
      res$intermediate$startingValues[1]  <-
        as.character(as.POSIXct(res$intermediate$startX,
                                origin = res$intermediate$day0));
      res$intermediate$lowerBounds[1] <-
        as.character(as.POSIXct(res$intermediate$changeInitiationBounds[1],
                                origin = res$intermediate$day0));
      res$intermediate$upperBounds[1] <-
        as.character(as.POSIXct(res$intermediate$changeInitiationBounds[2],
                                origin = res$intermediate$day0));
    }
    stop(paste0("\n\nWhile running genlog, an error in optimization ",
                "function nlsLM caught. This may ",
                "imply that the initial values you supplied are wrong, ",
                "or that too few data points are available to estimate ",
                "the parameters. The error was:\n\n",
                e, "\nThe starting and [constraining values] used were:\n\n  ",
                paste0(paste0(c("Change initiation:    ",
                                "Growth rate:          ",
                                "Curve bottom (floor): ",
                                "Curve top (ceiling):  ",
                                "V:                    "),
                              res$intermediate$startingValues, " [",
                              res$intermediate$lowerBounds, "; ",
                              res$intermediate$upperBounds,
                              "]"), collapse="\n  ")));
  });

  ### Extract coefficients
  res$output$maxChangeMoment <-
    x0 <- as.numeric(coef(out)[1]);
  res$output$growthRate <-
    B <-  as.numeric(coef(out)[2]);
  res$output$base <-
    Ab <- as.numeric(coef(out)[3]);
  res$output$top <-
    At <- as.numeric(coef(out)[4]);
  res$output$v <-
    v <-  as.numeric(coef(out)[5]);
  
  ### Compute effect size and fit information
  
  Dev <- deviance(out);
  validY <- !is.na(data[, yVar]);
  SSQtot <- sum((data[validY, yVar] - mean(data[validY, yVar])) ^ 2);
  res$output$deviance <- Dev;
  res$output$Rsq <-
    Rsq <- (SSQtot - Dev) / SSQtot;
  res$output$ES1 <-
    ES1 <- (At -Ab) / sd(data[, yVar]);
  res$output$ES2 <-
    ES2 <- (At -Ab) / diff(yRange);
  
  res$output$dat <-
    data.frame(deviance = Dev,
               Rsq = Rsq,
               ES1 = ES1,
               ES2 = ES2,
               growthRate = B,
               maxChangeMoment = x0,
               base = Ab,
               top = At);
  
  if (res$output$Rsq < 0) {
    warning(paste0("The fit of the generalized logistic function is *worse* ",
                   "than simply estimating the grand mean as best prediction ",
                   "for all observations. This suggests that the sigmoid ",
                   "model that the generalized logistic function attempts ",
                   "fit may not be appropriate for these data, or that the ",
                   "initial or boundary values should be adjusted. Inspect ",
                   "the data closely."));
  }
  
  yfit <- genlogFunction(x = data[, timeVar],
                         x0 = x0,
                         Ab = Ab,
                         At = At,
                         B = B,
                         v = v);

  interventionMoment <- mean(data[order(data[, timeVar],
                                        decreasing=FALSE)[c(baselineMeasurements,
                                                            baselineMeasurements+1)], timeVar]);
  
  if (!is.null(res$intermediate$day0)) {
    data[, timeVar] <-
      as.POSIXct(data[, timeVar], origin = res$intermediate$day0);
    x0 <- as.POSIXct(x0, origin = res$intermediate$day0);
    interventionMoment <- as.POSIXct(interventionMoment, origin = res$intermediate$day0);
  }
  
  if (is.null(plotLabs)) {
    plotLabs <- list(x = ifelse(is.null(res$intermediate$day0.formatted),
                                "Measurements",
                                "Date"),
                                #paste0("Days since ", res$intermediate$day0.formatted)),
                     y = yVar);
  }

  res$output$plot <-
    ggGenLogPlot(data,
                 timeVar = timeVar,
                 yVar = yVar,
                 phaseVar = phaseVar,
                 baselineMeasurements = baselineMeasurements,
                 ### These are all provided by genlogCompleteStartValues
                 yRange = res$intermediate$yRange,
                 startX = res$intermediate$startX,
                 startBase = res$intermediate$startBase,
                 startTop = res$intermediate$startTop,
                 startGrowthRate = res$intermediate$startGrowthRate,
                 startV = res$intermediate$startV,
                 changeInitiationBounds = res$intermediate$changeInitiationBounds,
                 growthRateBounds = res$intermediategrowthRateBounds,
                 baseBounds = res$intermediatebaseBounds,
                 topBounds = res$intermediatetopBounds,
                 vBounds = res$intermediatevBounds,
                 ### These are specified when calling this function
                 colors = colors,
                 alphas = alphas,
                 theme = theme,
                 pointSize = pointSize,
                 lineSize = lineSize,
                 initialValuesLineType = initialValuesLineType,
                 curveSizeMultiplier = curveSizeMultiplier,
                 plotLabs = plotLabs);
  
  res$output$plot <-
    res$output$plot +
    ### Add pre stable value (bottom/floor)
    geom_hline(yintercept=Ab,
               colour=colors$bottomBound,
               size=lineSize) +
    ### Add post stable value (top/ceiling)
    geom_hline(yintercept=At,
               colour=colors$topBound,
               size=lineSize) +
    ### Add moment of max change
    geom_vline(xintercept=x0,
               colour=colors$mid,
               size=lineSize) +
    ### Add sigmoid
    geom_line(data=data.frame(x=data[, timeVar], y=yfit),
              aes_string(x='x', y='y'),
              colour=colors$curve,
              size = lineSize * curveSizeMultiplier);
    
  if (!is.null(res$intermediate$day0)) {
    res$output$maxChangeMoment.numeric <-
      res$output$maxChangeMoment;
    res$output$maxChangeMoment <-
      as.POSIXct(res$output$maxChangeMoment,
                 origin = res$intermediate$day0);
    res$output$plot <-
      res$output$plot + scale_x_datetime(date_breaks="2 months",
                                         date_labels="%m-%Y");
  }
  
  if (!is.null(outputFile)) {
    ggsaveParameters <- c(list(filename = outputFile,
                               plot = res$output$plot,
                               width = outputWidth,
                               height = outputHeight),
                          ggsaveParams);
    do.call(ggsave, ggsaveParameters);
  }
  
  class(res) <- "genlog";
  
  return(res);
  
}

print.genlog <- function(x, digits=3, ...) {
  if (x$input$showPlot) {
    grid.newpage();
    grid.draw(x$output$plot);
  }

  if (x$intermediate$omittedCases > 0) {
    sampleInfo <- paste0("(N = ",
                         x$intermediate$usedCases,
                         "; removed ",
                         x$intermediate$omittedCases,
                         " cases with missing values)");
  } else {
    sampleInfo <- paste0("(N = ",
                         x$intermediate$originalCases,
                         ")");
  }
  
  cat0("Generalized Logistic Analysis ", sampleInfo, "\n\n",
       "Estimated sigmoid association between ",
       x$intermediate$timeVarName, " and ",
       x$intermediate$yVarName, ".\n\n");
  
  addAsterisk <- function(input, intermediate, name) {
    if (length(intermediate[[name]]) == 1) {
      return(ifelse(isTRUE(input[[name]] == intermediate[[name]]),
                    intermediate[[name]],
                    paste0(intermediate[[name]], "*")));
    } else {
      return(ifelse(isTRUE(input[[name]] == intermediate[[name]]),
                    paste0(formatCI(intermediate[[name]], digits=digits)),
                    paste0(formatCI(intermediate[[name]], digits=digits), "*")));
    }
  }
  
  startValueVars <- c('startX',
                      'startBase',
                      'startGrowthRate',
                      'startTop');
  startValues <- lapply(startValueVars,
                        addAsterisk,
                        input = x$input,
                        intermediate = x$intermediate);
  names(startValues) <- startValueVars;
  
  boundValueVars <- c('changeInitiationBounds',
                      'baseBounds',
                      'topBounds');
  boundValues <- lapply(boundValueVars,
                        addAsterisk,
                        input = x$input,
                        intermediate = x$intermediate);
  names(boundValues) <- boundValueVars;
  
  ### If we worked with dates, overwrite numeric value with pretty date
  if (!is.numeric(x$output$maxChangeMoment)) {
    startValues$startX <-
      as.character(as.POSIXct(x$intermediate$startX,
                              origin = x$intermediate$day0));
    boundValues$changeInitiationBounds <-
      paste0("[",
             paste0(as.character(as.POSIXct(x$intermediate$changeInitiationBounds,
                                            origin = x$intermediate$day0)), collapse="; "),
             "]");
    if (!isTRUE(x$input$startX == x$intermediate$startX)) {
      startValues$startX <-
        paste0(startValues$startX, "*");
      boundValues$changeInitiationBounds <-
        paste0(boundValues$changeInitiationBounds, "*");
    }
  }
  
  cat0("Parameter starting values [and constraints]:\n",
       "  Onset of change: ", startValues$startX,
       " ", boundValues$changeInitiationBounds, "\n",
       "  Curve base:      ", startValues$startBase,
       " ", boundValues$baseBounds, "\n",
       "  Growth rate:     ", startValues$startGrowthRate,
       " ", formatCI(x$input$growthRateBounds), "\n",
       "  Curve top:       ", startValues$startTop,
       " ", boundValues$topBounds, "\n",
       "  V parameter:     ", x$input$startV,
       " ", formatCI(x$input$vBounds), "\n",
       "\n",
       "Note: Asterisks (*) denote values that were not specified manually (but inferred by genlog).\n\n");
  cat0("Parameter estimates:\n\n",
       "  Curve base (plateau before change): ",
       round(x$output$base, digits=digits), "\n",
       "  Growth rate:                        ",
       round(x$output$growthRate, digits=digits), "\n",
       "  Change maximal at:                  ",
       ifelse(is.numeric(x$output$maxChangeMoment),
              round(x$output$maxChangeMoment, digits=digits),
              as.character(x$output$maxChangeMoment)), "\n",
       "  Curve top (plateau after change):   ",
       round(x$output$top, digits=digits),
       "\n\n");
  cat0("Model fit and effect sizes estimates:\n\n",
       "  Deviance:          ", round(x$output$deviance, digits=digits), "\n",
       "  R squared:         ", round(x$output$Rsq, digits=digits), "\n",
       "  Effect Size 1:     ", round(x$output$ES1, digits=digits), "\n",
       "  Effect Size 2:     ", round(x$output$ES2, digits=digits), "\n");
}
