regr <- function(formula, data=NULL, conf.level=.95, digits=2,
                 pvalueDigits = 3, coefficients=c("raw", "scaled"),
                 plot=FALSE, pointAlpha = .5,
                 collinearity = FALSE, influential = FALSE,
                 ci.method = c("widest", "r.con", "olkinfinn"),
                 ci.method.note = FALSE, env=parent.frame()) {

  dat <- data;
  
  ### Generate object to store input, intermediate outcomes, and results
  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());
  
  ### Extract variables from formula
  res$intermediate$variableNames <- all.vars(formula);
  
  ### Convert formula to a character string
  res$intermediate$formula.as.character <-
    paste0(as.character(formula)[c(2, 1, 3)], collapse=" ");
  
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
  
  ### Standardize numeric variables
  res$intermediate$dat.scaled <- res$intermediate$dat.raw;
  res$intermediate$dat.scaled[, sapply(res$intermediate$dat.scaled, is.numeric)] <-
    as.data.frame(scale(res$intermediate$dat.scaled[, sapply(res$intermediate$dat.scaled, is.numeric)]));

  ### Run and store lm objects
  res$intermediate$lm.raw <-
    lm(formula=res$intermediate$formula, data=res$intermediate$dat.raw);
  res$intermediate$lm.scaled <-
    lm(formula=res$intermediate$formula, data=res$intermediate$dat.scaled);
  
  ### R^2 confidence interval based on formula at
  ### http://www.danielsoper.com/statcalc3/calc.aspx?id=28
  res$intermediate$rsq <- rsq <- summary(res$intermediate$lm.raw)$r.squared;
  res$intermediate$k <- k <- length(res$intermediate$lm.raw$terms) - 1;
  res$intermediate$n <- n <- res$intermediate$lm.raw$df.residual + k + 1;
  res$intermediate$rsq.se <- sqrt((4*rsq*(1-rsq)^2*(n-k-1)^2)/
                                  ((n^2-1)*(3+n)));
  res$intermediate$rsq.t.crit <- qt(p=1-(1-conf.level)/2, df=n-k-1);
  res$output$rsq.ci.olkinfinn <- c(res$intermediate$rsq -
                                   res$intermediate$rsq.t.crit *
                                   res$intermediate$rsq.se,
                                 res$intermediate$rsq +
                                   res$intermediate$rsq.t.crit *
                                   res$intermediate$rsq.se);
  
  res$output$rsq.ci.olkinfinn <- ifelse(res$output$rsq.ci.olkinfinn < 0,
                                        0, res$output$rsq.ci.olkinfinn);

  res$output$rsq.ci.r.con <- psych::r.con(sqrt(rsq), n);
  res$output$rsq.ci.r.con <- ifelse(res$output$rsq.ci.r.con < 0,
                                    0, res$output$rsq.ci.r.con) ^ 2;

  if ("widest" %IN% ci.method) {
    res$output$rsq.ci <- ifelse(range(res$output$rsq.ci.r.con) > 
                                range(res$output$rsq.ci.olkinfinn),
                                res$output$rsq.ci.r.con,
                                res$output$rsq.ci.olkinfinn);
  } else if ("r.con" %IN% ci.method) {
    res$output$rsq.ci <- res$output$rsq.ci.r.con;
  } else {
    res$output$rsq.ci <- res$output$rsq.ci.olkinfinn;
  }
  
  
  ### Run confint on lm object
  res$intermediate$confint.raw <-
    confint(res$intermediate$lm.raw, level=conf.level);
  res$intermediate$confint.scaled <-
    confint(res$intermediate$lm.scaled, level=conf.level);
  
  ### Run lm.influence on lm object
  res$intermediate$lm.influence.raw <-
    lm.influence(res$intermediate$lm.raw);
  res$intermediate$lm.influence.scaled <-
    lm.influence(res$intermediate$lm.scaled);
  
  ### Get variance inflation factors and compute tolerances
  if (collinearity && (length(res$intermediate$variables) > 2)) {
    res$intermediate$vif.raw <- car::vif(res$intermediate$lm.raw);
    res$intermediate$vif.scaled <- car::vif(res$intermediate$lm.scaled);
    if (is.vector(res$intermediate$vif.raw)) {
      res$intermediate$tolerance.raw <- 1/res$intermediate$vif.raw;
      res$intermediate$tolerance.scaled <- 1/res$intermediate$vif.scaled;
    }
  }
  
  ### get summary for lm objects
  res$intermediate$summary.raw <- summary(res$intermediate$lm.raw);
  res$intermediate$summary.scaled <- summary(res$intermediate$lm.scaled);
  
  ### Generate output
  res$output$coef.raw <- cbind(data.frame(res$intermediate$confint.raw[, 1],
                                          res$intermediate$confint.raw[, 2]),
                               res$intermediate$summary.raw$coefficients);
  res$output$coef.scaled <- cbind(data.frame(res$intermediate$confint.scaled[, 1],
                                             res$intermediate$confint.scaled[, 2]),
                                  res$intermediate$summary.scaled$coefficients);
  
  names(res$output$coef.raw) <- names(res$output$coef.scaled) <-
    c(paste0(conf.level*100,"% CI, lo"),
      paste0(conf.level*100,"% CI, hi"),
      'estimate', 'se', 't', 'p');
  
  if (plot) {
    if (length(res$intermediate$variables_namesOnly) == 2) {
      res$output$plot <- ggplot(res$intermediate$dat.raw,
                                aes_string(y=res$intermediate$variables_namesOnly[1],
                                        x=res$intermediate$variables_namesOnly[2])) +
        geom_point(alpha = pointAlpha) + geom_smooth(method='lm') + theme_bw();
    } else if (length(res$intermediate$variables_namesOnly) == 3) {
      
      if (is.numeric(res$intermediate$dat.raw[, res$intermediate$variables_namesOnly[2]]) &&
          is.numeric(res$intermediate$dat.raw[, res$intermediate$variables_namesOnly[3]])) {
        ### Both numeric, so we treat the second one as moderator and compute the line
        
        predictorName <- res$intermediate$variables_namesOnly[2];
        moderatorName <- res$intermediate$variables_namesOnly[3];
        
        predictorMean <- mean(res$intermediate$dat.raw[, predictorName],
                              na.rm=TRUE);
        predictorSD <- sd(res$intermediate$dat.raw[, predictorName],
                          na.rm=TRUE);
        # loPredictorValue <- predictorMean - predictorSD;
        # hiPredictorValue <- predictorMean + predictorSD;
        loPredictorValue <- min(res$intermediate$dat.raw[, predictorName],
                                na.rm=TRUE);
        hiPredictorValue <- max(res$intermediate$dat.raw[, predictorName],
                                na.rm=TRUE);
        
        moderatorMean <- mean(res$intermediate$dat.raw[, moderatorName],
                              na.rm=TRUE);
        moderatorSD <- sd(res$intermediate$dat.raw[, moderatorName],
                          na.rm=TRUE);
        loModeratorValue <- moderatorMean - moderatorSD;
        hiModeratorValue <- moderatorMean + moderatorSD;
        
        intercept <- res$output$coef.raw['(Intercept)', 'estimate'];
        predictorSlope <- res$output$coef.raw[predictorName, 'estimate'];
        moderatorSlope <- res$output$coef.raw[moderatorName, 'estimate'];
        interactionSlope <- res$output$coef.raw[paste0(predictorName,
                                                       ":",
                                                       moderatorName),
                                                'estimate'];

        depVarValue_loPred_loMod <- intercept +
          loPredictorValue * predictorSlope +
          loModeratorValue * moderatorSlope +
          loPredictorValue * loModeratorValue * interactionSlope;
        depVarValue_loPred_hiMod <- intercept +
          loPredictorValue * predictorSlope +
          hiModeratorValue * moderatorSlope +
          loPredictorValue * hiModeratorValue * interactionSlope;
        depVarValue_hiPred_loMod <- intercept +
          hiPredictorValue * predictorSlope +
          loModeratorValue * moderatorSlope +
          hiPredictorValue * loModeratorValue * interactionSlope;
        depVarValue_hiPred_hiMod <- intercept +
          hiPredictorValue * predictorSlope +
          hiModeratorValue * moderatorSlope +
          hiPredictorValue * hiModeratorValue * interactionSlope;

        moderatorDat <- data.frame(x = c(loPredictorValue, loPredictorValue),
                                   xend = c(hiPredictorValue, hiPredictorValue),
                                   y = c(depVarValue_loPred_loMod, depVarValue_loPred_hiMod),
                                   yend = c(depVarValue_hiPred_loMod, depVarValue_hiPred_hiMod),
                                   modVal = c("Low", "High"));
        names(moderatorDat)[5] <- res$intermediate$variables_namesOnly[3];
        
        res$output$plot <- ggplot(res$intermediate$dat.raw,
                                  aes_string(y=res$intermediate$variables_namesOnly[1],
                                             x=res$intermediate$variables_namesOnly[2])) +
          geom_point(alpha = pointAlpha) +
          geom_smooth(method='lm') +
          theme_bw() +
          geom_segment(data = moderatorDat,
                       aes_string(x = 'x', xend = 'xend',
                                  y = 'y', yend = 'yend',
                                  group = res$intermediate$variables_namesOnly[3],
                                  color = res$intermediate$variables_namesOnly[3]),
                       size=1) +
          scale_color_viridis(discrete = TRUE);

      } else if ((is.numeric(res$intermediate$dat.raw[, res$intermediate$variables_namesOnly[2]]) &&
                 (is.factor(res$intermediate$dat.raw[, res$intermediate$variables_namesOnly[3]]))) ||
                 (is.numeric(res$intermediate$dat.raw[, res$intermediate$variables_namesOnly[3]]) &&
                 (is.factor(res$intermediate$dat.raw[, res$intermediate$variables_namesOnly[2]])))) {
        ### One of the predictors is a factor, so we jsut plot lines for each value
        if (is.numeric(res$intermediate$dat.raw[, res$intermediate$variables_namesOnly[2]])) {
          predictor <- res$intermediate$variables_namesOnly[2];
          moderator <- res$intermediate$variables_namesOnly[3];
        } else {
          predictor <- res$intermediate$variables_namesOnly[3];
          moderator <- res$intermediate$variables_namesOnly[2];
        }
        res$output$plot <- ggplot(res$intermediate$dat.raw,
                                  aes_string(y=res$intermediate$variables_namesOnly[1],
                                             x=predictor)) +
          geom_point(alpha = pointAlpha) +
          geom_smooth(method='lm') +
          theme_bw() +
          geom_smooth(aes_string(y=res$intermediate$variables_namesOnly[1],
                                 x=predictor,
                                 group=moderator,
                                 color=moderator),
                      method="lm",
                      se = FALSE) +
          scale_color_viridis(discrete = TRUE);        
      }
    } else {
      warning("You requested a plot, but for now plots are ",
              "only available for regression analyses with one predictor.");
    }
  }
  
  ### Use Z = (b1 - b2) / sqrt(SE_b1^2 + SE_b2^2) to test whether
  ### the coefficients differ; add arguments such as
  ### compareCoefficients=FALSE, p.adjust="fdr" to enable user to
  ### request & correct this.
  
  class(res) <- 'regr';
  return(res);

}

print.regr <- function(x, digits=x$input$digits,
                       pvalueDigits=x$input$pvalueDigits, ...) {

  cat(paste0("Regression analysis for formula: ",
             x$intermediate$formula.as.character, "\n\n",
             "Significance test of the entire model (all predictors together):\n",
             "  Multiple R-squared: [",
             round(x$output$rsq.ci[1], digits), ", ",
             round(x$output$rsq.ci[2], digits),
             "] (point estimate = ",
             round(x$intermediate$summary.raw$r.squared, digits),
             ", adjusted = ",
             round(x$intermediate$summary.raw$adj.r.squared, digits), ")",
             ifelse(x$input$ci.method.note, "*\n", "\n"),
             "  Test for significance: F[",
             x$intermediate$summary.raw$fstatistic[2], ", ",
             x$intermediate$summary.raw$fstatistic[3], "] = ",
             round(x$intermediate$summary.raw$fstatistic[1], digits),
             ", ", formatPvalue(pf(x$intermediate$summary.raw$fstatistic[1],
                                   x$intermediate$summary.raw$fstatistic[2],
                                   x$intermediate$summary.raw$fstatistic[3],
                                   lower.tail=FALSE), digits=pvalueDigits), "\n"));
  if ("raw" %in% x$input$coefficients) {
    cat("\nRaw regression coefficients (unstandardized beta values, called 'B' in SPSS):\n\n");
    tmpDat <- round(x$output$coef.raw[, 1:5], digits);
    tmpDat[[1]] <- paste0("[", tmpDat[[1]], "; ", tmpDat[[2]], "]");
    tmpDat[[2]] <- NULL;
    names(tmpDat)[1] <- paste0(x$input$conf.level*100, "% conf. int.");
    tmpDat$p <- formatPvalue(x$output$coef.raw$p,
                             digits=pvalueDigits,
                             includeP=FALSE);
    print(tmpDat, ...);
  }
  if ("scaled" %in% x$input$coefficients) {
    cat("\nScaled regression coefficients (standardized beta values, called 'Beta' in SPSS):\n\n");
    tmpDat <- round(x$output$coef.scaled[, 1:5], digits);
    tmpDat[[1]] <- paste0("[", tmpDat[[1]], "; ", tmpDat[[2]], "]");
    tmpDat[[2]] <- NULL;
    names(tmpDat)[1] <- paste0(x$input$conf.level*100, "% conf. int.");
    tmpDat$p <- formatPvalue(x$output$coef.scaled$p,
                             digits=pvalueDigits,
                             includeP=FALSE);
    print(tmpDat, ...);
  }
  
  if (x$input$collinearity && (!is.null(x$intermediate$vif.raw))) {
    cat0("\nCollinearity diagnostics:\n\n");
    if (is.vector(x$intermediate$vif.raw)) {
      cat0("  For the raw regression coefficients:\n\n");
      collinearityDat <- data.frame(VIF = x$intermediate$vif.raw,
                                    Tolerance = x$intermediate$tolerance.raw);
      row.names(collinearityDat) <- paste0(repStr(4), names(x$intermediate$vif.raw));
      print(collinearityDat);
      cat0("\n  For the standardized regression coefficients:\n\n");
      collinearityDat <- data.frame(VIF = x$intermediate$vif.scaled,
                                    Tolerance = x$intermediate$tolerance.scaled);
      row.names(collinearityDat) <- paste0(repStr(4), names(x$intermediate$vif.raw));
      print(collinearityDat);
    }
  }
  
  ciMsg <- "\n* Note that the confidence interval for R^2 is based on ";
  if (x$input$ci.method[1] == 'r.con') {
    ciMsg <- paste0(ciMsg,
                    "the confidence interval for the Pearson Correlation of ",
                    "the multiple correlation using r.con from the 'psych' ",
                    "package because that was specified using the 'ci.method' ",
                    "argument.");
  } else if (x$input$ci.method[1] == 'olkinfinn') {
    ciMsg <- paste0(ciMsg,
                    "the formula reported by Olkin and Finn (1995) in their Correlation ",
                    "Redux paper, because this was specified using the 'ci.method' ",
                    "argument. This may not work well for very low values. Set the ",
                    "argument to 'widest' to also compute the confidence interval ",
                    "of the multiple correlation using the r.con function from ",
                    "the 'psych' package and selecting the widest interval.");
  } else if (identical(x$output$rsq.ci, x$output$rsq.ci.r.con)) {
    ciMsg <- paste0(ciMsg,
                    "the confidence interval for the Pearson Correlation of ",
                    "the multiple correlation using r.con from the 'psych' ",
                    "package because that was the widest interval, which ",
                    "should be used because the 'ci.method' was set to 'widest'.");
  } else if (identical(x$output$rsq.ci, x$output$rsq.ci.olkinfinn)) {
    ciMsg <- paste0(ciMsg,
                    "the formula reported by Olkin and Finn (1995) in their Correlation ",
                    "Redux paper, because this was the widest interval, which ",
                    "should be used because the 'ci.method' was set to 'widest'.");
  } else {
    ciMsg <- paste0(ciMsg,
                    " -- I don't know actually, something appears to have gone wrong. ",
                    "The 'ci.method' argument was set to ", vecTxtQ(x$input$ci.method),
                    ".");
  }
  
  if (x$input$ci.method.note) {
    cat("\n");
    cat(strwrap(ciMsg), sep="\n");
  }
  
  if (!is.null(x$output$plot)) {
    print(x$output$plot);
  }
  invisible();
  
}

### Function to smoothly pander output from regr function in userfriendlyscience
pander.regr <- function (x, digits = x$input$digits, pvalueDigits = x$input$pvalueDigits, ...) {
  pandoc.p(paste0("\n\n#### Regression analysis for formula: ", x$intermediate$formula.as.character));
  pandoc.p("\n\n##### Significance test of the entire model (all predictors together):\n\n");
  pandoc.p(paste0("Multiple R-squared: [", round(x$output$rsq.ci[1], 
                                                 digits), ", ", round(x$output$rsq.ci[2], digits), 
                  "] (point estimate = ", round(x$intermediate$summary.raw$r.squared, 
                                                digits), ", adjusted = ", round(x$intermediate$summary.raw$adj.r.squared, 
                                                                                digits), ")"))
  pandoc.p(paste0("Test for significance: F[", x$intermediate$summary.raw$fstatistic[2], 
                  ", ", x$intermediate$summary.raw$fstatistic[3], "] = ", 
                  round(x$intermediate$summary.raw$fstatistic[1], digits), 
                  ", ", formatPvalue(pf(x$intermediate$summary.raw$fstatistic[1], 
                                        x$intermediate$summary.raw$fstatistic[2], x$intermediate$summary.raw$fstatistic[3], 
                                        lower.tail = FALSE), digits = pvalueDigits), "\n"));
  
  if ("raw" %in% x$input$coefficients) {
    pandoc.p("\n\n##### Raw regression coefficients (unstandardized beta values, called 'B' in SPSS):\n\n");
    tmpDat <- round(x$output$coef.raw[, 1:5], digits);
    tmpDat[[1]] <- paste0("[", tmpDat[[1]], "; ", tmpDat[[2]], "]");
    tmpDat[[2]] <- NULL;
    names(tmpDat)[1] <- paste0(x$input$conf.level * 100, "% conf. int.");
    tmpDat$p <- formatPvalue(x$output$coef.raw$p, digits = pvalueDigits, includeP = FALSE);
    pander(tmpDat, missing="");
  }
  if ("scaled" %in% x$input$coefficients) {
    pandoc.p("\n\n##### Scaled regression coefficients (standardized beta values, called 'Beta' in SPSS):\n\n");
    tmpDat <- round(x$output$coef.scaled[, 1:5], digits);
    tmpDat[[1]] <- paste0("[", tmpDat[[1]], "; ", tmpDat[[2]], "]");
    tmpDat[[2]] <- NULL;
    names(tmpDat)[1] <- paste0(x$input$conf.level * 100, "% conf. int.");
    tmpDat$p <- formatPvalue(x$output$coef.scaled$p, digits = pvalueDigits, includeP = FALSE);
    pander(tmpDat, missing="");
  }
}
