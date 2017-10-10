logRegr <- function(formula, data=NULL, conf.level=.95, digits=2,
                    pvalueDigits = 3,
                    crossTabs = TRUE,
                    plot=FALSE,
                    collinearity = FALSE,
                    env=parent.frame(),
                    predictionColor = viridis(3)[3],
                    predictionAlpha = .5,
                    predictionSize = 2,
                    dataColor = viridis(3)[1],
                    dataAlpha = .33,
                    dataSize=2,
                    observedMeansColor = viridis(3)[2],
                    binObservedMeans = 7,
                    observedMeansSize = 2,
                    observedMeansWidth = NULL,
                    observedMeansAlpha = .5,
                    theme=theme_bw()) {

  ### Generate object to store input, intermediate outcomes, and results
  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());
  
  ### Extract variables from formula
  res$intermediate$variableNames <- all.vars(formula);
  
  ### Convert formula to a character string
  res$intermediate$formula.as.character <-
    paste0(as.character(formula)[c(2, 1, 3)], collapse=" ");
  
  dat <- data;
  
  if (is.null(dat)) {
    ### Extract variables from formula
    res$intermediate$variables <-
      as.character(as.list(attr(terms(formula), 'variables'))[-1]);
    
    ### Store variablesnames only for naming in raw dataframe
    res$intermediate$variables_namesOnly <- unlist(
      lapply(strsplit(res$intermediate$variables, "\\$"), tail, 1));
    
    ### Store variables in a dataframe
    res$intermediate$dat.raw <- list();
    for (varToGet in 1:length(res$intermediate$variables)) {
      res$intermediate$dat.raw[[res$intermediate$variables_namesOnly[varToGet]]] <-
        eval(parse(text=res$intermediate$variables[varToGet]), envir=env);
    }
    ### Convert the list to a dataframe
    res$intermediate$dat.raw <- data.frame(res$intermediate$dat.raw);
    
    ### Convert variable names to bare variable names
    for (currentVariableIndex in 1:length(res$intermediate$variables)) {
      res$intermediate$formula.as.character <-
        gsub(res$intermediate$variables[currentVariableIndex],
             res$intermediate$variables_namesOnly[currentVariableIndex],
             res$intermediate$formula.as.character, fixed=TRUE);  
    }

  } else {
    ### Store variables in a dataframe
    res$intermediate$dat.raw <- dat[, res$intermediate$variableNames];
    res$intermediate$variables_namesOnly <- res$intermediate$variableNames;
  }

  res$intermediate$formula <- formula(res$intermediate$formula.as.character,
                                      env = environment());
  
  ### Run and store lm objects
  res$intermediate$glm <-
    glm(formula=res$intermediate$formula,
        data=res$intermediate$dat.raw,
        family=binomial(link='logit'));

  ### Test difference from intercept-only model
  ###   (written by Ron Pat-El)
  res$intermediate$modelChi <-
    res$intermediate$glm$null.deviance -
    res$intermediate$glm$deviance;
  res$intermediate$chiDf <- res$intermediate$glm$df.null -
    res$intermediate$glm$df.residual;
  res$intermediate$deltaChisq <-
    1 - pchisq(res$intermediate$modelChi, res$intermediate$chiDf);

  ### Calculate Cox & Snell R-squared and NagelKerke R-squared
  ###   (also written by Ron Pat-El)
  res$intermediate$CoxSnellRsq <-
    1 - exp((res$intermediate$glm$deviance -
             res$intermediate$glm$null.deviance) / 
            length(res$intermediate$glm$fitted.values));
  res$intermediate$NagelkerkeRsq <-
    res$intermediate$CoxSnellRsq /
    (1 - exp(-(res$intermediate$glm$null.deviance /
               length(res$intermediate$glm$fitted.values))));
  

  ### Run confint on lm object
  res$intermediate$confint <-
    confint(res$intermediate$glm, level=conf.level);

  ### Run lm.influence on lm object
  res$intermediate$influence <-
    influence(res$intermediate$glm);

  ### Get variance inflation factors and compute tolerances
  if (collinearity && (length(res$intermediate$variables) > 2)) {
    res$intermediate$vif <- car::vif(res$intermediate$glm);
    if (is.vector(res$intermediate$vif)) {
      res$intermediate$tolerance <- 1/res$intermediate$vif;
    }
  }
  
  ### get summary for lm objects
  res$intermediate$summary <- summary(res$intermediate$glm);

  ### Generate output
  res$output$coef <- cbind(data.frame(res$intermediate$confint[, 1],
                                      res$intermediate$confint[, 2]),
                           res$intermediate$summary$coefficients);

  names(res$output$coef) <-
    c(paste0(conf.level*100,"% CI, lo"),
      paste0(conf.level*100,"% CI, hi"),
      'estimate', 'se', 'z', 'p');

  ######################################################################
  ### Make the prediction tables
  ######################################################################
  
  res$intermediate$predictedY.raw <-
    predict(res$intermediate$glm);
  
  ### Convert to probability
  res$intermediate$predictedY.prob <- exp(res$intermediate$predictedY.raw) /
    (1 + exp(res$intermediate$predictedY.raw));
  
  ### Round to 0 or 1
  res$intermediate$predictedY.dichotomous <-
    round(res$intermediate$predictedY.prob);
  
  res$output$crossTab.model <- table(res$intermediate$dat.raw[, 1],
                                     res$intermediate$predictedY.dichotomous,
                                     dnn=c("Observed", "Predicted"));

  ### Best predictions on the basis of the null model is either 0 or 1
  if (mean(res$intermediate$dat.raw[, 1], na.rm=TRUE) < .5) {
    res$intermediate$predictedY.null <- rep(0, nrow(res$intermediate$dat.raw));
  } else {
    res$intermediate$predictedY.null <- rep(1, nrow(res$intermediate$dat.raw));
  }
  
  ### Build crosstables
  res$output$crossTab.model <- table(res$intermediate$dat.raw[, 1],
                                     res$intermediate$predictedY.dichotomous,
                                     dnn=c("Observed", "Predicted"));
  res$output$crossTab.null <- table(res$intermediate$dat.raw[, 1],
                                    res$intermediate$predictedY.null,
                                    dnn=c("Observed", "Predicted"));
  
  ### Compute the proportion of correct predictions for the null model
  ### and the real model
  res$output$proportionCorrect.model <- sum(diag(res$output$crossTab.model)) /
    sum(res$output$crossTab.model);
  res$output$proportionCorrect.null <- sum(diag(res$output$crossTab.null)) /
    sum(res$output$crossTab.null);
  
  if (plot) {
    if (length(res$intermediate$variables_namesOnly) == 2) {

      ### Get a vector of equally distributed values for predictor
      res$intermediate$plotDat <-
        data.frame(x = seq(from = min(res$intermediate$dat.raw[, 2]),
                           to = max(res$intermediate$dat.raw[, 2]),
                           length.out=10000));
      names(res$intermediate$plotDat) <- res$intermediate$variables_namesOnly[2];
      
      ### Get predicted log odds and add them to the dataframe
      res$intermediate$predictedData <- 
        predict(res$intermediate$glm,
                newdata=res$intermediate$plotDat,
                se.fit=TRUE);
      res$intermediate$plotDat$predictedLogOdds <-
        res$intermediate$predictedData$fit;
      res$intermediate$plotDat$predictionSE <-
        res$intermediate$predictedData$se.fit;
      
      convertLogOdds <- function(x) {
        return(exp(x) / (1 + exp(x)));
      }
      
      ### Convert to odds and add confidence intervals
      zValue <- qnorm(1 - (1-conf.level)/2);
      res$intermediate$plotDat$predictedProbability <-
        convertLogOdds(res$intermediate$plotDat$predictedLogOdds);
      res$intermediate$plotDat$ci.lo <-
        convertLogOdds(res$intermediate$plotDat$predictedLogOdds -
                         (zValue * res$intermediate$plotDat$predictionSE));
      res$intermediate$plotDat$ci.hi <-
        convertLogOdds(res$intermediate$plotDat$predictedLogOdds +
                         (zValue * res$intermediate$plotDat$predictionSE));
      
      ### Create dataframe for observed values
      res$intermediate$plotDatObserved <- res$intermediate$dat.raw;
      tmpDat <- res$intermediate$dat.raw;
      if (is.numeric(binObservedMeans)) {

        tmpDat[, res$intermediate$variables_namesOnly[2]] <-
          cut(tmpDat[, res$intermediate$variables_namesOnly[2]],
              breaks=binObservedMeans);
        
        res$intermediate$binCenters <-
          levels(tmpDat[, res$intermediate$variables_namesOnly[2]]);
        
        res$intermediate$binCenters <- substr(res$intermediate$binCenters,
                                              2,
                                              nchar(res$intermediate$binCenters) - 1);
        
        res$intermediate$binCenters <- sapply(strsplit(res$intermediate$binCenters, ","),
                                              function(x) return(mean(as.numeric(x))));

        names(res$intermediate$binCenters) <-
          levels(tmpDat[, res$intermediate$variables_namesOnly[2]]);
        tmpDat[, res$intermediate$variables_namesOnly[2]] <-
          res$intermediate$binCenters[tmpDat[, res$intermediate$variables_namesOnly[2]]];

      }
      
      res$intermediate$plotDatObservedMeanPrep <- tmpDat;
      
      res$intermediate$plotDatObservedMean <-
        ddply(tmpDat,
              res$intermediate$variables_namesOnly[2],
              function(x) {
                return(mean(x[, res$intermediate$variables_namesOnly[1]], na.rm=TRUE));
              });
      names(res$intermediate$plotDatObservedMean) <- c('x', 'y');

      if (is.null(observedMeansWidth)) {
        observedMeansWidth <- (diff(range(res$intermediate$dat.raw[, 1])));
      }
      
      res$intermediate$plotDatObservedMean$minX <-
        res$intermediate$plotDatObservedMean$x - (observedMeansWidth / 2)
      res$intermediate$plotDatObservedMean$maxX <-
        res$intermediate$plotDatObservedMean$x + (observedMeansWidth / 2)
      
      ### Compute jitter parameters
      jitterWidth <-
        findShortestInterval(res$intermediate$plotDatObserved[, res$intermediate$variables_namesOnly[2]]) / 2;
        
      res$output$plot <- ggplot(res$intermediate$plotDat,
                                aes_string(y = 'predictedProbability',
                                           x = res$intermediate$variables_namesOnly[2])) +
        geom_ribbon(mapping=aes_string(ymin = 'ci.lo',
                                       ymax = 'ci.hi'),
                    fill=predictionColor, alpha=predictionAlpha) +
        geom_line(color=predictionColor, size=predictionSize) +
        geom_jitter(data = res$intermediate$plotDatObserved,
                    aes_string(x=res$intermediate$variables_namesOnly[2],
                               y=res$intermediate$variables_namesOnly[1]),
                    color=dataColor,
                    alpha=dataAlpha,
                    width=jitterWidth,
                    size = dataSize,
                    height=.025) +
        geom_segment(data=res$intermediate$plotDatObservedMean,
                     aes_string(x='minX', y='y',
                                xend='maxX', yend='y'),
                     color=observedMeansColor,
                     size=observedMeansSize,
                     alpha=observedMeansAlpha) +
        theme;
      
      # res$output$plot <- ggplot(res$intermediate$dat.raw,
      #                           aes_string(y=res$intermediate$variables_namesOnly[1],
      #                                   x=res$intermediate$variables_namesOnly[2])) +
      #   geom_point(alpha = pointAlpha) + geom_smooth(method='lm') + theme_bw();
    } else {
      warning("You requested a plot, but for now plots are ",
              "only available for logistic regression ",
              "analyses with one predictor.");
    }
  }
  
  class(res) <- 'logRegr';
  return(res);

}

print.logRegr <- function(x, digits=x$input$digits,
                          pvalueDigits=x$input$pvalueDigits, ...) {

  cat0("Logistic regression analysis for formula: ",
       x$intermediate$formula.as.character, "\n\n",
       "Significance test of the entire model (all predictors together):\n",
       "  Cox & Snell R-squared: ",
       round(x$intermediate$CoxSnellRsq, digits),
       ",\n",
       "  Nagelkerke R-squared: ",
       round(x$intermediate$NagelkerkeRsq, digits),
       "\n",
       "  Test for significance: ChiSq[",
       x$intermediate$chiDf,
       "] = ",
       round(x$intermediate$modelChi, digits),
       ", ",
       formatPvalue(x$intermediate$deltaChisq, digits=pvalueDigits), "\n");
  
  if (x$input$crossTabs) {
    cat0("\nPredictions by the null model (",
         round(100 * x$output$proportionCorrect.null, digits),
         "% correct):\n\n");
    print(x$output$crossTab.null);
    
    cat0("\nPredictions by the tested model (",
         round(100 * x$output$proportionCorrect.model, digits),
         "% correct):\n\n");
    print(x$output$crossTab.model);
  }
  
  cat("\nRaw regression coefficients (log odds values, called 'B' in SPSS):\n\n");
  tmpDat <- round(x$output$coef[, 1:5], digits);
  tmpDat[[1]] <- paste0("[", tmpDat[[1]], "; ", tmpDat[[2]], "]");
  tmpDat[[2]] <- NULL;
  names(tmpDat)[1] <- paste0(x$input$conf.level*100, "% conf. int.");
  tmpDat$p <- formatPvalue(x$output$coef$p,
                           digits=pvalueDigits,
                           includeP=FALSE);
  print(tmpDat, ...);

  if (x$input$collinearity && (!is.null(x$intermediate$vif))) {
    cat0("\nCollinearity diagnostics:\n\n");
    if (is.vector(x$intermediate$vif)) {
      collinearityDat <- data.frame(VIF = x$intermediate$vif,
                                    Tolerance = x$intermediate$tolerance);
      row.names(collinearityDat) <- paste0(repStr(4), names(x$intermediate$vif));
      print(collinearityDat);
    }
  }
  
  if (!is.null(x$output$plot)) {
    print(x$output$plot);
  }
  invisible();
  
}
