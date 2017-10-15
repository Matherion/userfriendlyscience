fanova <- function(data,
                   y,
                   between = NULL,
                   covar = NULL,
                   plot = FALSE,
                   levene = FALSE,
                   digits = 2,
                   contrast = NULL) {

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  res$intermediate$dataName <- as.character(deparse(substitute(data)));
  
  if (length(y) == 1) {
    res$intermediate$yVarName <- y;
  } else {
    res$intermediate$yVarName <- sharedSubString(y);
    if (is.na(res$intermediate$yVarName))
      res$intermediate$yVarName <- vecTxt(y);
  }
  
  res$output$msg <- paste0("Flexible Analysis of Variance was called with:\n\n",
                           "  Dependent variable: ", res$intermediate$yVarName, "\n",
                           ifelse(is.null(between), "", paste0("  Factors: ", vecTxt(between), "\n")),
                           ifelse(is.null(covar), "", paste0("  Covariates: ", vecTxt(covar), "\n")),
                           "\n");

  ### Set contrast function; first set default contrast for repeated
  ### measures anova
  if (is.null(contrast)) {
    contrast <- 'poly';
  }
  if (!is.null(contrast)) {
    contrastFunction <- paste0('contr.', contrast);
    if (!exists(contrastFunction)) {
      stop("Function doesn't exist");
    }
  }

  ### Convert 'between' variables (factors) to factors
  for (currentVar in between[!is.factor(data[, between])]) {
    res$output$msg <- paste0(res$output$msg,
                             "Between-subjects factor '", currentVar,
                             "' does not have class 'factor' in dataframe '",
                             res$intermediate$dataName, "'. Converting it now.\n");
    data[, currentVar] <- factor(data[, currentVar]);
  }
    
  if (length(y) == 1) {
    ### This is a simple anova; check whether it's oneway, factorial,
    ### or ancova
    if (is.null(covar) && (length(between) == 1)) {
      y <- data[, y];
      x <- data[, between];
      res$intermediate$formula <- paste(y, "~", between);
      res$intermediate$secondaryObject <-
        userfriendlyscience::oneway(y = y,
                                    x = x,
                                    plot=plot,
                                    levene=levene);
      ### Get plot and store it here
      if (plot)
        res$output$plot <-
          res$intermediate$secondaryObject$output$plot;
    } else {
      ### Factorial anova or ancova
      
      ### Generate formula
      res$intermediate$formula <-
        formula(paste(y, "~", paste(c(between, covar), collapse="*")));
      
      ### Run linear model
      res$intermediate$primaryObject <-
        lm(formula=res$intermediate$formula,
           data = data,
           contrasts = contrastFunction);
      
      ### Get Anova results using car's Anova
      res$intermediate$secondaryObject <-
        car::Anova(res$intermediate$primaryObject, type=3);
      
      ### Make a plot if we want one
      if (plot) {
        if (is.null(covar) && (length(between) == 2)) {
          res$output$plot <- dlvPlot(data,
                                     y=y,
                                     x=between[1],
                                     z=between[2])$plot +
            ggtitle(paste0(between[1], ", ",
                           between[2], " and ",
                           y));
        } else {
          warning("Sorry, I can only generate a plot for oneway or ",
                  "two-way anovas (not for ancovas or anovas with ",
                  "more than two factors).");
        }
      }
      
      ### Optional Levene's test
      if (levene) {
        leveneY <- data[, y];
        leveneGroups <-
          as.factor(apply(data[, between], 1, function(x) return(paste0(x, collapse="-"))));
        res$intermediate$leveneTest <-
          car::leveneTest(leveneY, group=leveneGroups, center=mean);
      }
      
    }
  } else {
    ### We need to do a repeated measures anova, so first convert the
    ### data to a long format.
    longDat <- data.frame(subject = factor(rep(row.names(data), length(y))),
                          time = factor(rep(seq_along(y), each=nrow(data))),
                          y = unlist(data[, y]));
    for (currentVar in between) {
      longDat[, currentVar] <- factor(rep(data[, currentVar],
                                          length(y)));
    }
    for (currentVar in covar) {
      longDat[, currentVar] <- as.numeric(rep(data[, currentVar],
                                              length(y)));
    }
    
    res$intermediate$longDat <- longDat;
    
    ### Then build the lmer formula: y is predicted by all
    ### interactions and main effects for all factors ('between')
    ### and covariates ('covar'), all interactions between all
    ### factors and the time variable (called 'time'), and
    ### the random slope for time (the final term).

    res$intermediate$formula <-
      formula(paste("y ~", paste(c(between, covar), collapse=" * "),
                    "+", paste(c(between, "time"), collapse=" * "),
                    "+ (1|time)"));
    
    ### Run the mixed model
    res$intermediate$primaryObject <-
      lmer(formula=res$intermediate$formula,
           data = longDat,
           contrasts = contrastFunction);
    
    ### Run the analysis of variance
    suppressMessages(res$intermediate$secondaryObject <-
      car::Anova(res$intermediate$primaryObject,
                 type=3, test.statistic="F"));
    
    ### Approach using lm (and the idata and idesign objects for Anova)
    # idata <- longDat; #[, 'time', drop=FALSE];
    # print(names(longDat))
    # res$intermediate$formula <- formula(paste0("cbind(", paste(y, collapse=", "), ") ~",
    #                                            paste(c(between, covar), collapse=" * ")));
    # res$intermediate$primaryObject <- lm(res$intermediate$formula,
    #                                      data = data);
    # print(res$intermediate$primaryObject);
    # print(idata);
    # res$intermediate$secondaryObject <- Anova(res$intermediate$primaryObject,
    #                                           idata=idata,
    #                                           idesign=~1*time,
    #                                           type=3,
    #                                           test.statistic='F');
    
    if (plot) {
      if (length(between) > 1) {
        warning("Sorry, I can only generate a plot for ",
                "oneway repeated measures anovas.");
      } else {
        res$output$plot <-
          dlvPlot(longDat,
                  x='time',
                  y='y',
                  z=between)$plot +
          labs(x='Time', y=res$intermediate$yVarName);
      }
    }

  }


  class(res) <- 'fanova';
  return(res);
  
}

print.fanova <- function(x, digits=x$input$digits, ...) {
  cat(x$output$msg);
  cat("\n");
  if (!is.null(x$output$plot)) {
    grid.newpage();
    grid.draw(x$output$plot);
  }
  print(x$intermediate$secondaryObject);
  
  if(!is.null(x$intermediate$leveneTest)) {
    cat0("\n### Levene's test for homogeneity of variance:\n\n",
         "F[", x$intermediate$leveneTest[1, 1],
         ", ", x$intermediate$leveneTest[2, 1],
         "] = ", round(x$intermediate$leveneTest[1, 2], digits),
         ", ", formatPvalue(x$intermediate$leveneTest[1, 3], digits=digits+1),
         ".\n");
  }
  
}
