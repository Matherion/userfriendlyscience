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
                          showPlot = TRUE,
                          outputFile = NULL,
                          outputWidth = 16,
                          outputHeight = 16,
                          ggsaveParams = list(units='cm',
                                              dpi=300,
                                              type="cairo")) {

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  dat <- data;
  
  if (is.null(phaseVar)) {
    if (is.null(baselineMeasurements)) {
      stop("You did not specify a 'phaseVar' and you also did ",
           "not specify how many baselineMeasurements there are. ",
           "I need one of those to know when the intervention ",
           "occurred.");
    } else {
      phaseVar <- 'phaseVar';
      dat[, phaseVar] <- c(rep(0, baselineMeasurements),
                           rep(1, nrow(dat) - baselineMeasurements));
    }
  }
  
  dat <- dat[, c(timeVar, yVar, phaseVar)];

  ### Remove cases with missing values
  dat <- dat[complete.cases(dat), ];
  
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
  
  ### Tc is adjusted to start with 0
  dat[, timeVar] <- dat[, timeVar] - min(dat[, timeVar]);
  
  ### Store in result object
  res$intermediate$dat <- dat;
  
  ### Get baselineMeasurements in case we didn't have it yet
  res$intermediate$baselineMeasurements <- nA <-
    sum(dat[, phaseVar] == min(dat[, phaseVar]));
  
  ### Store sample size
  res$intermediate$n <- n <- nrow(dat);
  
  ### Trend term for phase B  (see Huitema & Kean, 2000)
  dat$trendTerm <- ifelse(dat[, phaseVar] == max(dat[, phaseVar]),
                          dat[, timeVar] - dat[nA + 1, timeVar],
                          0);
  
  ## Construct formula
  lmFormula <-
    as.formula(paste(yVar, "~", phaseVar, "+", timeVar, "+ trendTerm"));

  if (robust) { 
    ### Fit piecewise model and extract Huber weights
    res$intermediate$rlm <- rlm(lmFormula, data=dat);
    res$intermediate$weights <- weights <- res$intermediate$rlm$w;
  } else {
    ### Set all weights to 1 (every datapoint has equal value)
    res$intermediate$weights <- weights <- rep(1, n);
  }

  ### Fit piecewise model
  res$intermediate$lm.model <-
    lm(lmFormula, data=dat, weights = weights);
  res$intermediate$lm.null <-
    lm(as.formula(paste(yVar, "~", timeVar)), data=dat, weights = weights);
  
  ### Extract R squared values
  res$output$Rsq.null <- rsq0 <-
    summary(res$intermediate$lm.null)$r.squared;
  res$output$Rsq.model <- rsq1 <-
    rsq1 <- summary(res$intermediate$lm.model)$r.squared;
  
  ### compute Effect Size, see Parker & Brossart (2003, p.207)
  res$output$ES <- ES <- (rsq1 - rsq0)/(1-rsq0);

  ### Extract coefficients
  res$output$coef <- coefficients(res$intermediate$lm.model);
  
  ### Generate plot; first dataframes for the 'custom lines'
  ypre <- res$output$coef[1] + res$output$coef[3] * dat[, timeVar];
  ypost <- res$intermediate$lm.model$fitted.values;
  predictionDf1 <- data.frame(x = dat[nA, timeVar],
                              xend = dat[nA+1, timeVar],
                              y = ypre[nA],
                              yend = ypre[nA+1]);
  predictionDf2 <- data.frame(x = dat[nA+1, timeVar],
                              xend = dat[nA+1, timeVar],
                              y = ypre[nA+1],
                              yend = ypost[nA+1]);

  res$output$plot <- plot <-
    ggplot(data = dat,
           aes_string(x = timeVar,
                      y = yVar)) +
    geom_vline(xintercept = mean(c(dat[nA, timeVar],
                                   dat[nA+1, timeVar])),
               colour = colors$intervention,
               size=lineSize) +
    geom_smooth(data = dat[dat[, phaseVar] == min(dat[, phaseVar]), ],
                method='lm',
                color = colors$pre,
                fill = colors$pre,
                size=lineSize) +
    geom_smooth(data = dat[dat[, phaseVar] == max(dat[, phaseVar]), ],
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
    theme +
    labs(x = ifelse(is.null(res$intermediate$day0.formatted),
                    "Measurements",
                    paste0("Days since ", res$intermediate$day0.formatted)),
         y = yVar);

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
  cat0("Piecewise Regression Analysis\n",
       "\nModel statistics:\n",
       "\n  R squared for null model:    ", formatR(x$output$Rsq.null, digits),
       "\n  R squared for test model:    ", formatR(x$output$Rsq.model, digits),
       "\n  R squared based effect size: ", formatR(x$output$ES, digits),
       "\n\nRegression coefficients:\n",
       "\n  Intercept:       ", round(x$output$coef[1], digits),
       "\n  Level change:    ", round(x$output$coef[2], digits),
       "\n  Trend phase 1:   ", round(x$output$coef[3], digits),
       "\n  Change in trend: ", round(x$output$coef[4], digits));
}

