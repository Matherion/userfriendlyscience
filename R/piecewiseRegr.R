piecewiseRegr <- function(data,
                          timeVar = 1,
                          yVar = 2,
                          phaseVar = NULL,
                          baselineMeasurements = NULL, ### Was nA
                          robust = FALSE,
                          digits=2,
                          colors = list(pre = viridis(4)[1],
                                        post = viridis(4)[4],
                                        diff = viridis(4)[3],
                                        intervention = viridis(4)[2],
                                        points = "black"),
                          theme = theme_minimal(),
                          pointSize = 2,
                          pointAlpha = 1,
                          lineSize = 1,
                          yRange=range(data[, yVar], na.rm=TRUE),
                          yBreaks = 1,
                          showPlot = TRUE,
                          plotLabs = NULL,
                          outputFile = NULL,
                          outputWidth = 16,
                          outputHeight = 16,
                          ggsaveParams = list(units='cm',
                                              dpi=300,
                                              type="cairo")) {

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  if (is.null(phaseVar)) {
    if (is.null(baselineMeasurements)) {
      stop("You did not specify a 'phaseVar' and you also did ",
           "not specify how many baselineMeasurements there are. ",
           "I need one of those to know when the intervention ",
           "occurred.");
    } else {
      phaseVar <- 'phaseVar';
      data[, phaseVar] <- c(rep(0, baselineMeasurements),
                           rep(1, nrow(data) - baselineMeasurements));
    }
  }
  
  ### In case people specify one or more indices
  if (is.numeric(timeVar)) timeVar <- names(data)[timeVar];
  if (is.numeric(yVar)) yVar <- names(data)[yVar];
  if (is.numeric(phaseVar)) phaseVar <- names(data)[phaseVar];
  
  data <- data[, c(timeVar, yVar, phaseVar)];

  ### Check data types of all variables
  # if (!(is.numeric(data[, timeVar]))) {
  #   warning("Time variable is not a numeric variable (but instead has class ",
  #           vecTxtQ(class(data[, timeVar])),
  #           ")! However, since the time variable will be the ",
  #           "predictor in a regression analysis, it *must* be ",
  #           "numeric. I'm trying to covert it myself now.");
  #   data[, timeVar] <- as.numeric(data[, timeVar]);
  # }
  if (!(is.numeric(data[, yVar]))) {
    warning("The y variable is not a numeric variable (but instead has class ",
            vecTxtQ(class(data[, yVar])),
            ")! However, since the y variable will be the ",
            "dependent variable in a regression analysis, it *must* be ",
            "numeric. I'm trying to covert it myself now.");
    data[, yVar] <- as.numeric(data[, yVar]);
  }
  if (!(is.numeric(data[, phaseVar])) && !(is.factor(data[, phaseVar]))) {
    warning("The phase variable is not a numeric variable or a ",
            "factor (but instead has class ",
            vecTxtQ(class(data[, phaseVar])),
            ")! However, since I must be able to determine what the first ",
            "phase is, it must have one of those two classes. ",
            "I'm trying to covert it to a factor myself now.");
    data[, phaseVar] <- factor(data[, phaseVar]);
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
      res$intermediate$day0 <- min(data[, timeVar]);
      res$intermediate$day0.formatted <- as.character(res$intermediate$day0);
      data[, timeVar] <- as.numeric(data[, timeVar]) - as.numeric(min(data[, timeVar]));
    } else {
      stop("The timeVar variable does not have a class I can work with (numeric or date): instead it has class ",
           vecTxtQ(class(data[, timeVar])), ".");
    }
  }
  
  ### Tc is adjusted to start with 0
  data[, timeVar] <- data[, timeVar] - min(data[, timeVar]);
  
  ### Store in result object
  res$intermediate$dat <- data;
  
  ### Get minimum and maximum of phase variable
  if (is.factor(data[, phaseVar])) {
    phaseVarMin <- min(levels(data[, phaseVar]));
    phaseVarMax <- max(levels(data[, phaseVar]));
  } else {
    phaseVarMin <- min(data[, phaseVar]);
    phaseVarMax <- max(data[, phaseVar]);
  }
  
  ### Get baselineMeasurements in case we didn't have it yet
  res$intermediate$baselineMeasurements <- nA <-
    sum(data[, phaseVar] == phaseVarMin);
  
  ### Store sample size
  res$intermediate$n <- n <- nrow(data);
  
  ### Trend term for phase B  (see Huitema & Kean, 2000)
  data$trendTerm <- ifelse(data[, phaseVar] == phaseVarMax,
                          data[, timeVar] - data[nA + 1, timeVar],
                          0);

  ## Construct formula
  lmFormula <-
    as.formula(paste(yVar, "~", phaseVar, "+", timeVar, "+ trendTerm"));

  if (robust) { 
    ### Fit piecewise model and extract Huber weights
    res$intermediate$rlm <- rlm(lmFormula, data=data);
    res$intermediate$weights <- weights <- res$intermediate$rlm$w;
  } else {
    ### Set all weights to 1 (every datapoint has equal value)
    res$intermediate$weights <- weights <- rep(1, n);
  }

  ### Fit piecewise model
  res$intermediate$lm.model <-
    lm(lmFormula, data=data, weights = weights);
  res$intermediate$lm.null <-
    lm(as.formula(paste(yVar, "~", timeVar)), data=data, weights = weights);
  
  ### Extract R squared values
  res$output$Rsq.null <- rsq0 <-
    summary(res$intermediate$lm.null)$r.squared;
  res$output$Rsq.model <- rsq1 <-
    rsq1 <- summary(res$intermediate$lm.model)$r.squared;
  
  ### compute Effect Size, see Parker & Brossart (2003, p.207)
  res$output$ES <- ES <- (rsq1 - rsq0)/(1-rsq0);

  ### Extract coefficients
  res$output$coef <- coefficients(res$intermediate$lm.model);
  
  ### Add confidence intervals
  res$output$confint <- confint(res$intermediate$lm.model);
  
  ### Add deviance
  res$output$deviance <- deviance(res$intermediate$lm.model);
  
  ### Add "Cohen's D" (simply difference in means divided by standard deviation)
  res$intermediate$meanDiff <- meanDiff(x=data[, phaseVar],
                                        y=data[, yVar]);

  ### Generate plot; first dataframes for the 'custom lines'
  ypre <- res$output$coef[1] + res$output$coef[3] * data[, timeVar];
  ypost <- res$intermediate$lm.model$fitted.values;
  predictionDf1 <- data.frame(x = data[nA, timeVar],
                              xend = data[nA+1, timeVar],
                              y = ypre[nA],
                              yend = ypre[nA+1]);
  predictionDf2 <- data.frame(x = data[nA+1, timeVar],
                              xend = data[nA+1, timeVar],
                              y = ypre[nA+1],
                              yend = ypost[nA+1]);

  if (!is.null(res$intermediate$day0)) {
    data[, timeVar] <-
      as.POSIXct(data[, timeVar], origin = res$intermediate$day0);
    predictionDf1$x <-
      as.POSIXct(predictionDf1$x, origin = res$intermediate$day0);
    predictionDf1$xend <-
      as.POSIXct(predictionDf1$xend, origin = res$intermediate$day0);
    predictionDf2$x <-
      as.POSIXct(predictionDf2$x, origin = res$intermediate$day0);
    predictionDf2$xend <-
      as.POSIXct(predictionDf2$xend, origin = res$intermediate$day0);
  }
  
  if (is.null(plotLabs)) {
    plotLabs <- list(x = ifelse(is.null(res$intermediate$day0.formatted),
                                "Measurements",
                                "Date"),
                     #paste0("Days since ", res$intermediate$day0.formatted)),
                     y = yVar);
  }

  res$output$plot <- plot <-
    ggplot(data = data,
           aes_string(x = timeVar,
                      y = yVar)) +
    geom_vline(xintercept = mean(c(data[nA, timeVar],
                                   data[nA+1, timeVar])),
               colour = colors$intervention,
               size=lineSize) +
    geom_smooth(data = data[data[, phaseVar] == phaseVarMin, ],
                method='lm',
                color = colors$pre,
                fill = colors$pre,
                size=lineSize) +
    geom_smooth(data = data[data[, phaseVar] == phaseVarMax, ],
                method='lm',
                color = colors$post,
                fill = colors$post,
                size=lineSize) +
    geom_segment(data = predictionDf1,
                 aes_string(x = 'x',
                            xend = 'xend',
                            y = 'y',
                            yend = 'yend'),
                 color = colors$pre,
                 size=lineSize,
                 linetype = 'dotted') +
    geom_segment(data = predictionDf2,
                 aes_string(x = 'x',
                            xend = 'xend',
                            y = 'y',
                            yend = 'yend'),
                 color = colors$diff,
                 size=lineSize) +    
    geom_point(size = pointSize,
               alpha = pointAlpha,
               color = colors$points) +
    scale_y_continuous(breaks=seq(from = min(yRange),
                                  to = max(yRange),
                                  by= yBreaks)) +
    coord_cartesian(ylim=yRange) +
    theme +
    do.call(labs, plotLabs);
  
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
  
  class(res) <- 'piecewiseRegr';
  return(res);
  
}

print.piecewiseRegr <- function(x,
                                digits=x$input$digits,
                                ...) {
  if (x$input$showPlot) {
    grid.newpage();
    grid.draw(x$output$plot);
  }
  
  confIntervals <- c(formatCI(x$output$confint[1, ]),
                     formatCI(x$output$confint[2, ]),
                     formatCI(x$output$confint[3, ]),
                     formatCI(x$output$confint[4, ]));
  
  maxConfIntLength <- max(nchar(confIntervals));
  
  confIntervals <- paste0(sapply(maxConfIntLength - nchar(confIntervals), repStr), confIntervals);

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

  cat0("Piecewise Regression Analysis ", sampleInfo, "\n",
       "\nModel statistics:\n",
       "\n  Model deviance:              ", round(x$output$deviance, digits),
       "\n  R squared for null model:    ", formatR(x$output$Rsq.null, digits),
       "\n  R squared for test model:    ", formatR(x$output$Rsq.model, digits),
       "\n  R squared based effect size: ", formatR(x$output$ES, digits),
       "\n\nRegression coefficients:\n",
       "\n  Intercept:       ", confIntervals[1], " (point estimate = ", round(x$output$coef[1], digits), ")",
       "\n  Level change:    ", confIntervals[2], " (point estimate = ", round(x$output$coef[2], digits), ")",
       "\n  Trend phase 1:   ", confIntervals[3], " (point estimate = ", round(x$output$coef[3], digits), ")",
       "\n  Change in trend: ", confIntervals[4], " (point estimate = ", round(x$output$coef[4], digits), ")");
}

