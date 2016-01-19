# require(pwr)
# require(MASS)
# require(mosaic)
# require(psych)
# regrPwrSim(100, predictors=3)
# 
# 
# regrPwrSim(n = 200, samples = 100,
#            cor=matrix(c( 1, .3, .4,
#                         .3,  1, .3,
#                         .4, .3,  1), ncol=3, byrow=TRUE),
#            predictorNames = c('severity',
#                               'selfdestruction'),
#            dependentName = 'treatmentSatisfaction');
# 
# Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_45') # for 64-bit version
# 
# require(rJava);
# 
# regSim(rep(0,3), matrix(c( 1, .3, .4,
#                    .3,  1, .2,
#                    .4, .2,  1), ncol=3, byrow=TRUE));

# http://www.uic.edu/classes/psych/psych548/fraley/powerregressionr.htm

### Power simulations for regression analyses using roughly equally
### correlated predictors and one dependent variable; interaction terms
### specified in a named list, with each list element being a vector
### specifying the terms in the interaction.

regrPwrSim <- function(n, predictors=NULL, cor = c(.3, .5), 
                       predictorNames = paste0("predictor_", 1:predictors),
                       dependentName = "dependent", 
                       samples=100, sig.level=.05, interactions = NULL,
                       digits=2, means=0, empirical=FALSE) {
  
  res <- list(intermediate = list(cor = cor),
              output = list());
  
  if (is.vector(cor) && (length(cor) == 2)) {
    if (is.null(predictors)) {
      stop("If you don't supply a correlation matrix in the 'cor' argument, ",
           "you have to specify the number of predictors in the 'predictors' ",
           "argument!");
    }
    res$intermediate$cor <- data.frame(matrix(cor[1], nrow=predictors,
                                              ncol=predictors));
    res$intermediate$cor[, predictors+1] <- cor[2];
    res$intermediate$cor[predictors+1, ] <- cor[2];
    diag(res$intermediate$cor) <- 1;
  }
  else if (!is.matrix(cor) && (ncol(cor) == nrow(cor)) &&
             (ncol(cor)== predictors)) {
    stop("The 'cor' argument has to be either a vector with two elements, ",
         "specifying the correlation between the predictors with each other ",
         "and the dependent variable, or a matrix representing the ",
         "correlations.");
  }
  else {
    predictors <- (ncol(cor) - 1);
  }

  res$input <- as.list(environment());
    
  colnames(res$intermediate$cor) <- rownames(res$intermediate$cor) <- c(predictorNames, dependentName);
  
  res$intermediate$predictorCorrelations <-
    res$intermediate$cor[-nrow(res$intermediate$cor), -ncol(res$intermediate$cor)];
  
  res$intermediate$meanPredictorsCor <-
    mean(res$intermediate$predictorCorrelations[lower.tri(res$intermediate$predictorCorrelations)]);
  
  res$intermediate$regressionAnalyses <-
    do(samples) * function(mu = rep(means, (predictors+1)),
                           Sigma = res$intermediate$cor,
                           predictNames = predictorNames,
                           dependName = dependentName,
                           sampleSize = n,
                           interactionTerms = interactions,
                           emp = empirical) {
      
      res <- list();
      
      dat <- data.frame(mvrnorm(n = sampleSize, mu = mu, Sigma = Sigma,
                        empirical=emp));
      names(dat) <- c(predictNames, dependName);
      regressionFormula <- paste(dependName, "~",
                                  paste0(predictNames, collapse=" + "));

      ### Build regression equations for the partial correlations
      ### (see also http://stats.stackexchange.com/questions/76815/multiple-regression-or-partial-correlation)
      res$correctedPredictorRegressions <- list();
      res$correctedDependentRegressions <- list();
      res$uniquePredictorPortion <- list();
      res$correctedCorrelations <- list();
      for (currentPredictor in predictNames) {
        
        ### First build a model for and do a regression where this predictor
        ### has all variance explained by other predictors partialled out
        res$correctedPredictorRegressions[[currentPredictor]] <- list();
        res$correctedPredictorRegressions[[currentPredictor]]$formula <-
          paste(currentPredictor, "~",
                paste0(predictNames[predictName != currentPredictor],
                       collapse=" + "));
        res$correctedPredictorRegressions[[currentPredictor]]$lm <-
          lm(formula(res$correctedPredictorRegressions[[currentPredictor]]$formula), dat);
        
        ### Then store those residuals
        res$uniquePredictorPortion[[currentPredictor]] <-
          residuals(res$correctedPredictorRegressions[[currentPredictor]]$lm);
        
        ### Then remove the variance explained by the other predictors
        ### out of the dependent variable (i.e. store the residuals from
        ### a regression analysis where we predict the dependent variable
        ### by all other predictors)
        res$correctedDependentRegressions[[currentPredictor]] <- list();
        res$correctedDependentRegressions[[currentPredictor]]$formula <-
          paste(dependName, "~",
                paste0(predictNames[predictName != currentPredictor],
                       collapse=" + "));
        res$correctedDependentRegressions[[currentPredictor]]$lm <-
          lm(formula(res$correctedDependentRegressions[[currentPredictor]]$formula), dat);
        
        ### Then store those residuals
        res$uniqueDependentPortion[[currentPredictor]] <-
          residuals(res$correctedDependentRegressions[[currentPredictor]]$lm);
        
        ### Finally, correlate the corrected predictor with both the
        ### raw and the corrected dependent variable
        res$correctedCorrelations[[currentPredictor]]$cor.pred_raw_dep <-
          cor(res$uniquePredictorPortion[[currentPredictor]], dat[[dependent]]);
        res$correctedCorrelations[[currentPredictor]]$cor.pred_cor_dep <-
          cor(res$uniquePredictorPortion[[currentPredictor]],
              res$uniqueDependentPortion[[currentPredictor]]);
        
        ### And add the power for these two
        res$correctedCorrelations[[currentPredictor]]$pwr.pred_raw_dep <-
          pwr.r.test(r = res$correctedCorrelations[[currentPredictor]]$cor.pred_raw_dep,
                     n = sampleSize);
        res$correctedCorrelations[[currentPredictor]]$pwr.pred_cor_dep <-
          pwr.r.test(r = res$correctedCorrelations[[currentPredictor]]$cor.pred_cor_dep,
                     n = sampleSize);
        
      }
      
      if (!is.null(interactionTerms)) {
        for (interaction in 1:length(interactionTerms)) {
          if (!(min(interactionTerms[[interaction]]) < 1 ||
                  !(min(interactionTerms[[interaction]]) > predictors))) {
            stop("The lowest number in the list provided as argument 'interactions' ",
                 "is lower than 1 or higher than the number of predictors!");
          }
          if (max(interactionTerms[[interaction]]) > predictors) {
            stop("The highest number in the list provided as argument 'interactions' ",
                 "is higher than the number of predictors!");
          }
          dat[, names(interactionTerms)[interaction]] <-
            dat[, dependName] *
            dat[, interactionTerms[[interaction]][1]] *
            dat[, interactionTerms[[interaction]][2]];
        }
        regressionFormula <- paste(regressionFormula, "+",
                                   paste(names(interactionTerms), collapse = " + "));
      }
      
      regressionOutcome <- lm(formula(regressionFormula), dat);
      
      res$significances <- tail(summary(regressionOutcome)$coefficients[,'Pr(>|t|)'], -1);
      res$correlations <- cor(dat);
      
      print(summary(regressionOutcome)$coefficients);
      
      return(res);
    };
  
  print(res$intermediate$regressionAnalyses$correctedCorrelations);
  
  ### Convert significances to dataframe
  res$intermediate$regressionAnalyses$significances <-
    t(data.frame(res$intermediate$regressionAnalyses$significances));
  row.names(res$intermediate$regressionAnalyses$significances) <- NULL;
  
  ### Convert correlations to Fisher's Z's
  res$intermediate$fishersZ <- lapply(res$intermediate$regressionAnalyses$correlations, fisherz);
  res$intermediate$meanFishersZ <- Reduce("+", res$intermediate$fishersZ) / length(res$intermediate$fishersZ);
  res$intermediate$generatedCorrelations <-
    fisherz2r(res$intermediate$meanFishersZ);
  
  diag(res$intermediate$generatedCorrelations) <- 1;
  
  res$intermediate$significant <- apply(res$intermediate$regressionAnalyses$significances,
                                        2, function(x) { sum(x < sig.level); }) / samples;
  
  res$intermediate$bivariatePower <- sapply(res$intermediate$generatedCorrelations[c(predictorNames, names(interactions)), dependentName],
                                            function(x, n) {
                                              return(pwr.r.test(r=x, n=n, sig.level=sig.level)$power);
                                            }, n=n);
  
  res$intermediate$bivariatePower.adjusted <- sapply(res$intermediate$generatedCorrelations[c(predictorNames, names(interactions)), dependentName],
                                            function(x, n) {
                                              return(pwr.r.test(r=x, n=n, sig.level=sig.level/predictors)$power);
                                            }, n=n);
  
  res$output$dat <- data.frame(bivar.cor = res$intermediate$generatedCorrelations[c(predictorNames, names(interactions)), dependentName],
                               regr.pwr = res$intermediate$significant,
                               cor.pwr = res$intermediate$bivariatePower,
                               cor.pwr.adj = res$intermediate$bivariatePower.adjusted,
                               );
  
  row.names(res$output$dat) <- c(predictorNames, names(interactions));
  
  class(res) <- 'regrPwrSim';
  
  return(res);
  
}

print.regrPwrSim <- function(res, digits=res$input$digits, ...) {
  cat0("Ran ", res$input$samples, " regression analyses, each with a sample size of ",
       res$input$n, " and ", res$input$predictors, " predictors, with an average ",
      "correlation between predictors of ",
      res$intermediate$meanPredictorsCor, " and an average correlation between ",
      "predictors and the criterion of ", mean(res$output$dat$biVarCorrelation),
      ". You provided this correlation matrix:\n\n");
  print(res$intermediate$cor);
  
  if (!is.null(res$input$interactions)) {
    cat0("\nAnd you specified these interaction terms:\n",
         paste0("  ", names(res$input$interactions),
                ": ",
                lapply(res$input$interactions, function(indices, names) {
                  return(vecTxt(names[indices]));
                },
                res$input$predictorNames), "\n"));
  }
  
  cat0("\nFrom which I generated ", res$input$samples,
       " samples with correlation matrices that average (through Fisher's z) to:\n\n");
  
  print(res$intermediate$generatedCorrelations, digits=digits);
  cat0("\nThese are the bivariate correlations, achieved power, and the power ",
       "for the bivariate correlations, for your specified sample size of ",
       res$input$n, ":\n\n");
  print(res$output$dat, digits=digits);
}