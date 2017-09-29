### Optional Levene's test

### Add plot

### 

fanova <- function(data,
                   y,
                   between = NULL,
                   covar = NULL,
                   contrast = NULL,
                   plot = FALSE,
                   ...) {

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
                                    x = x);
    } else {
      ### Factorial anova or ancova
      res$intermediate$formula <-
        formula(paste(y, "~", paste(c(between, covar), collapse="*")));
      res$intermediate$primaryObject <-
        lm(formula=res$intermediate$formula,
           data = data,
           contrasts = contrastFunction);
      res$intermediate$secondaryObject <-
        car::Anova(res$intermediate$primaryObject, type=3);
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
  }
  
  # if (plot) {
  #   if (length(y) == 1)
  #   
  #   
  #   res$intermediate$plotDat <- ifelseObj(length(y) == 1,
  #                                         dat,
  #                                         
  #                                     
  #     data.frame(x, y);
  #   names(res$intermediate$dat) <- c(res$input$x.name, res$input$y.name);
  #   res$output$plot <- dlvPlot(res$intermediate$dat,
  #                              x=res$input$x.name,
  #                              y=res$input$y.name)$plot +
  #     ggtitle(paste0(res$input$x.name, " and ",
  #                    res$input$y.name));
  # }
  

  class(res) <- 'fanova';
  return(res);
  
}

print.fanova <- function(x, ...) {
  cat(x$output$msg);
  cat("\n");
  print(x$intermediate$secondaryObject);
}

# require(lme4);
# require(userfriendlyscience);
# require(car);
### see https://stats.stackexchange.com/questions/26810/why-isnt-the-anova-function-in-the-car-package-returning-an-f-statistic

# CBM <- read.csv("http://userfriendlyscience.com/files/cbm.csv",
#                 sep=";", dec=",");

# CBM <- read.csv("B:/Data/teaching/OU/workshops/R/R for beginners/cbm.csv",
#                 sep=";", dec=",");
# 
# fanova(dat=Orange, y='circumference', between='Tree')
# 
# fanova(data=CBM, y="rt_parallel_boven_v1",
#        between=c('Sekse', 'BekendheidCBM'));
# 
# fanova(data=CBM, y="rt_parallel_boven_v1",
#        between=c('BekendheidCBM'),
#        covar="Leeftijd");
# 
# fanova(data=CBM,
#        y=c("rt_parallel_boven_v1", "rt_parallel_boven_v2", "rt_parallel_boven_v3"),
#        between='Sekse');

