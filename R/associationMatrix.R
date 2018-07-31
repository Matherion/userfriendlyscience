###########################################################
### Define main functions
###########################################################

### First, we define the functions that will compute the
### results. We have functions for the different combinations
### of measurement levels of the two variables.

### Function for the t-test
computeStatistic_t <- function(var1, var2, conf.level=.95,
                               var.equal='test', ...) {

  if (nlevels(as.factor(var1)) == 2) {
    dichotomous <- factor(var1);
    interval <- var2;
  } else if (nlevels(as.factor(var2)) == 2) {
    dichotomous <- factor(var2);
    interval <- var1;
  } else {
    stop("Error: none of the two variables has only two levels!");
  }

  res <- list();
  res$object <- meanDiff(interval ~ dichotomous, conf.level = conf.level,
                         envir=environment(), var.equal=var.equal);
  res$statistic <- res$object$t;
  res$statistic.type <- "t";
  res$parameter <- res$object$df;
  res$p.raw <- res$object$p;
  return(res);
}

### Function for the Pearson correlation (r)
computeStatistic_r <- function(var1, var2, conf.level=.95,
                               ...) {
  res <- list();
  res$object <- cor.test(var1, var2, use="complete.obs");
  res$statistic <- res$object$statistic;
  res$statistic.type <- "r";
  res$parameter <- res$object$parameter;
  res$p.raw <- res$object$p.value;
  return(res);
}

### Function for Anova (f)
computeStatistic_f <- function(var1, var2, conf.level=.95,
                               ...) {

  if (is.factor(var1) & is.numeric(var2)) {
    factor <- var1;
    dependent <- var2;
  } else if (is.factor(var2) & is.numeric(var1)) {
    factor <- var2;
    dependent <- var1;
  } else if (nlevels(as.factor(var1)) < nlevels(as.factor(var2))) {
    ## We will treat var1 as factor
    factor <- factor(var1);
    dependent <- as.numeric(var2);
  }
  else {
    factor <- factor(var2);
    dependent <- as.numeric(var1);
  }

  ### In the future perhaps include tests of
  ### homogeneity of variances:
  #bartlett.test(dependent ~ factor);
  #fligner.test(dependent ~ factor);
  #safeRequire('car');
  #levene.test(dependent ~ factor);

  res <- list();
  res$object <- aov(dependent ~ factor);

  res$statistic <- summary(res$object)[[1]][['F value']][1];
  res$statistic.type <- "f";
  res$parameter <- c(summary(res$object)[[1]][['Df']]);
#                    summary(res$object)[[2]][['Df']]);
  res$p.raw <- summary(res$object)[[1]][['Pr(>F)']][1];
  return(res);
}

### Function for chi-square (chisq)
computeStatistic_chisq <- function(var1, var2, conf.level=.95,
                                   ...) {
  res <- list();
  res$object <- chisq.test(var1, var2, correct=FALSE);
  res$statistic <- res$object$statistic;
  res$statistic.type <- "chisq";
  res$parameter <- res$object$parameter;
  res$p.raw <- res$object$p.value;
  return(res);
}

### Effect size Cohens d
computeEffectSize_d <- function(var1, var2, conf.level=.95,
                                var.equal="test", ...) {
  if (length(unique(na.omit(var1))) == 2) {
    dichotomous <- factor(var1);
    interval <- var2;
  }
  else if (length(unique(na.omit(var2))) == 2) {
    dichotomous <- factor(var2);
    interval <- var1;
  }
  else {
    stop("Error: none of the two variables has only two levels!");
  }
  res <- list();
  res$object <- meanDiff(interval ~ dichotomous, conf.level = conf.level,
                         var.equal=var.equal, envir=environment());
  res$es <- res$object$meanDiff.g;
  res$es.type <- "g";
  res$ci <- c(res$object$meanDiff.g.ci.lower,
              res$object$meanDiff.g.ci.upper);
  return(res);
}

### Effect size Pearson's r
computeEffectSize_r <- function(var1, var2, conf.level=.95,
                                ...) {
  res <- list();
  res$object <- cor.test(var1, var2, use="complete.obs");
  res$es <- res$object$estimate;
  res$es.type <- "r";
  res$ci <- res$object$conf.int;
  return(res);
}

### Function for eta squared (etasq)
computeEffectSize_etasq <- function(var1, var2, conf.level=.95,
                                    ...) {

  if (is.factor(var1) & is.numeric(var2)) {
    factor <- var1;
    dependent <- var2;
  }
  else if (is.factor(var2) & is.numeric(var1)) {
    factor <- var2;
    dependent <- var1;
  } else if (nlevels(as.factor(var1)) < nlevels(as.factor(var2))) {
    ## We will treat var1 as factor
    factor <- factor(var1);
    dependent <- as.numeric(var2);
  }
  else {
    factor <- factor(var2);
    dependent <- as.numeric(var1);
  }

  res <- list();

  ### Confidence level should be doubled (i.e. unconfidence level
  ### should be doubled to be more precise), so .95 becomes .90 ->
  ### see http://daniellakens.blogspot.nl/2014/06/calculating-confidence-intervals-for.html
  ### for a brief explanation and links to more extensive explanations.

  res$realConfidence <- 1 - ((1-conf.level) * 2);

  res$object.aov <- aov(dependent ~ factor);

  df_num <- summary(res$object.aov)[[1]][1,1];
  df_den <- summary(res$object.aov)[[1]][2,1];
  f_val <- summary(res$object.aov)[[1]][1,4];

  ### This is suggested by the page at
  ### http://yatani.jp/HCIstats/ANOVA#RCodeOneWay
  ### (capture.output used because this function for
  ###  some reason very tenaciously outputs results)
  ### (also note that we double the 'unconfidence' level,
  ###  e.g. conf.level=.95 becomes conf.level=.90, to
  ###  retain consistency with the NHST p-value; see
  ###  the Word doc by Karl Wuensch references above,
  ###  or the paper he cites:
  ###    Steiger, J. H. (2004). Beyond the F test:
  ###      Effect size confidence intervals and tests
  ###      of close fit in the analysis of variance and
  ###      contrast analysis. Psychological methods, 9(2),
  ###      164-82. doi:10.1037/1082-989X.9.2.164

  res$es <- df_num*f_val/(df_den + df_num*f_val);
  res$es.type <- "etasq";
  capture.output(res$object <- ci.pvaf(F.value=f_val, df.1=df_num, df.2=df_den,
                        N=(df_den+df_num+1), conf.level=res$realConfidence));

  res$ci <- c(res$object$Lower.Limit.Proportion.of.Variance.Accounted.for,
              res$object$Upper.Limit.Proportion.of.Variance.Accounted.for);

  return(res);
}


### Function for omega squared (etasq)
computeEffectSize_omegasq <- function(var1, var2, conf.level=.95,
                                      ...) {

  res$object <- confIntOmegaSq(var1, var2, conf.level=conf.level);

  res$es <- res$object$output$es;
  res$es.type <- "omegasq";
  res$ci <- res$object$output$ci;

  return(res);
}

### Function for Cramers V effect size (v)
computeEffectSize_v <- function(var1, var2, conf.level=.95,
                                bootstrap=FALSE, samples=5000,
                                ...) {
  res <- list();
  if (bootstrap) {
    res$object <- confIntV(var1, var2,
                           method="bootstrap",
                           samples=samples);
    res$ci <- res$object$output$confIntV.bootstrap;
  } else {
    res$object <- confIntV(var1, var2, method="fisher");
    res$ci <- res$object$output$confIntV.fisher;
  }
  res$es <- res$object$intermediate$cramersV$output$cramersV
  res$es.type <- "V";
  return(res);
}

### This is the function that calls the functions
### to compute statistics and effect sizes, and
### organises the resulting objects in sets of
### lists. The elements of the first list are
### the 'rows' of the matrix. Each element (each
### 'row') is itself again a list, where each
### element corresponds to a 'cell' in the
### final 'matrix'. Each of these element (each
### of these cells) contains two objects; the one
### containing the statistic and the one
### containing the effect size.

associationMatrixStatDefaults <- list(dichotomous =
                                        list(dichotomous = "computeStatistic_chisq",
                                             nominal = "computeStatistic_chisq",
                                             ordinal = "computeStatistic_chisq",
                                             numeric = "computeStatistic_t"),
                                      nominal =
                                        list(dichotomous = "computeStatistic_chisq",
                                             nominal = "computeStatistic_chisq",
                                             ordinal = "computeStatistic_chisq",
                                             numeric = "computeStatistic_f"),
                                      ordinal =
                                        list(dichotomous = "computeStatistic_chisq",
                                             nominal = "computeStatistic_chisq",
                                             ordinal = "computeStatistic_chisq",
                                             numeric = "computeStatistic_f"),
                                      numeric =
                                        list(dichotomous = "computeStatistic_t",
                                             nominal = "computeStatistic_f",
                                             ordinal = "computeStatistic_f",
                                             numeric = "computeStatistic_r"));
associationMatrixESDefaults <- list(dichotomous =
                                      list(dichotomous = "computeEffectSize_v",
                                           nominal = "computeEffectSize_v",
                                           ordinal = "computeEffectSize_v",
                                           numeric = "computeEffectSize_d"),
                                    nominal =
                                      list(dichotomous = "computeEffectSize_v",
                                           nominal = "computeEffectSize_v",
                                           ordinal = "computeEffectSize_v",
                                           numeric = "computeEffectSize_etasq"),
                                    ordinal =
                                      list(dichotomous = "computeEffectSize_v",
                                           nominal = "computeEffectSize_v",
                                           ordinal = "computeEffectSize_v",
                                           numeric = "computeEffectSize_etasq"),
                                    numeric =
                                      list(dichotomous = "computeEffectSize_d",
                                           nominal = "computeEffectSize_etasq",
                                           ordinal = "computeEffectSize_etasq",
                                           numeric = "computeEffectSize_r"));



#' associationMatrix
#' 
#' associationMatrix produces a matrix with confidence intervals for effect
#' sizes, point estimates for those effect sizes, and the p-values for the test
#' of the hypothesis that the effect size is zero, corrected for multiple
#' testing.
#' 
#' 
#' @param dat A dataframe with the variables of interest. All variables in this
#' dataframe will be used if both x and y are NULL. If dat is NULL, the user
#' will be presented with a dialog to select a datafile.
#' @param x If not NULL, this should be a character vector with the names of
#' the variables to include in the rows of the association table. If x is NULL,
#' all variables in the dataframe will be used.
#' @param y If not NULL, this should be a character vector with the names of
#' the variables to include in the columns of the association table. If y is
#' NULL, the variables in x will be used for the columns as well (which
#' produces a symmetric matrix, similar to most correlation matrices).
#' @param conf.level Level of confidence of the confidence intervals.
#' @param correction Correction for multiple testing: an element out of the
#' vector c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr",
#' "none").  NOTE: the p-values are corrected for multiple testing; The
#' confidence intervals are not!
#' @param bootstrapV Whether to use bootstrapping to compue the confidence
#' interval for Cramer's V or whether to use the Fisher's Z conversion.
#' @param info Information to print: either both the confidence interval and
#' the point estimate for the effect size (and the p-value, corrected for
#' multiple testing), or only the confidence intervals, or only the point
#' estimate (and the corrected p-value). Must be on element of the vector
#' c("full", "ci", "es").
#' @param includeSampleSize Whether to include the sample size when the effect
#' size point estimate and p-value are shown. If this is "depends", it will
#' depend on whether all associations have the same sample size (and the sample
#' size will only be printed when they don't). If "always", the sample size
#' will always be added.  If anything else, it will never be printed.
#' @param bootstrapV.samples If using boostrapping for Cramer's V, the number
#' of samples to generate.
#' @param digits Number of digits to round to when printing the results.
#' @param pValueDigits How many digits to use for formatting the p values.
#' @param colNames If true, the column heading will use the variables names
#' instead of numbers.
#' @param type Type of output to generate: must be an element of the vector
#' c("R", "html", "latex").
#' @param file If a file is specified, the output will be written to that file
#' instead of shown on the screen.
#' @param statistic This is the complicated bit; this is where
#' associationMatrix allows customization of the used statistics to perform
#' null hypothesis significance testing. For everyday use, leaving this at the
#' default value, associationMatrixStatDefaults, works fine. In case you want
#' to customize, read the 'Notes' section below.
#' @param effectSize Like the 'statistics' argument, 'effectSize also allows
#' customization, in this case of the used effect sizes. Again, the default
#' value, associationMatrixESDefaults, works for everyday use. Again, see the
#' 'Notes' section below if you want to customize.
#' @param var.equal Whether to test for equal variances ('test'), assume
#' equality ('yes'), or assume unequality ('no'). See \code{\link{meanDiff}}
#' for more information.
#' @return
#' 
#' An object with the input and several output variables, one of which is a
#' dataframe with the association matrix in it. When this object is printed,
#' the association matrix is printed to the screen. If the 'file' parameter is
#' specified, a file with this matrix will also be written to disk.
#' @note
#' 
#' The 'statistic' and 'effectSize' parameter make it possible to use different
#' functions to conduct null hypothesis significance testing and compute effect
#' sizes. In both cases, the parameter needs to be a list containing four
#' lists, named 'dichotomous', 'nominal', 'ordinal', and 'interval'. Each of
#' these lists has to contain four elements, character vectors of length one
#' (i.e.  just one string value), again named 'dichotomous', 'nominal',
#' 'ordinal', and 'interval'.
#' 
#' The combination of each of these names (e.g.  'dichotomous' and 'nominal',
#' or 'ordinal' and 'interval', etc) determine which test should be done when
#' computing the p-value to test the association between two variables of those
#' types, or which effect sizes to compute. When called, associationMatrix
#' determines the measurement levels of the relevant variables. It then uses
#' these two levels (their string representation, e.g. 'dichotomous' etc) to
#' find a string in the 'statistic' and 'effectSize' objects. Two functions
#' with these names are then called from two lists, 'computeStatistic' and
#' computeEffectSize. These lists list contain functions that have the same
#' names as the strings in the 'statistic' list.
#' 
#' For example, when the default settings are used, the string (function name)
#' found for two dichotomous variables when searching in
#' associationMatrixStatDefaults is 'chisq', and the string found in
#' associationMatrixESDefaults is 'v'.  associationMatrix then calls
#' computeStatistic[['chisq']] and computeEffectSize[['v']], providing the two
#' variables as arguments, as well as passing the 'conf.level' argument. These
#' two functions then each return an object that associationMatrix extracts the
#' information from. Inspect the source code of these functions (by typing
#' their names without parentheses in the R prompt) to learn how this object
#' should look, if you want to write your own functions.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utilities univar
#' @examples
#' 
#' 
#' ### Generate a simple association matrix using all three variables in the
#' ### Orange tree dataframe
#' associationMatrix(Orange);
#' 
#' ### Or four variables from infert:
#' associationMatrix(infert, c("education", "parity",
#'                             "induced", "case"), colNames=TRUE);
#' 
#' ### Use variable names in the columns and generate html
#' associationMatrix(Orange, colNames=TRUE, type='html');
#' 
#' 
#' @export associationMatrix
associationMatrix <- function(dat=NULL, x=NULL, y=NULL, conf.level = .95,
                              correction = "fdr", bootstrapV=FALSE,
                              info=c("full", "ci", "es"),
                              includeSampleSize = "depends",
                              bootstrapV.samples = 5000, digits = 2,
                              pValueDigits=digits + 1, colNames = FALSE,
                              type=c("R", "html", "latex"), file="",
                              statistic = associationMatrixStatDefaults,
                              effectSize = associationMatrixESDefaults,
                              var.equal = "test") {

  ### Make object to store results
  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());
  res$intermediate$statistics <- list();
  res$intermediate$effectSizes <- list();
  res$intermediate$sampleSizes <- list();

  ### If no dataframe was specified, load it from an SPSS file
  if (is.null(dat)) {
    dat <- getData(errorMessage=paste0("No dataframe specified, and no valid datafile selected in ",
                                       "the dialog I then showed to allow selection of a dataset.",
                                       "Original error:\n\n[defaultErrorMessage]"),
                   use.value.labels=FALSE);
    res$input$dat.name <- paste0("SPSS file imported from ", attr(dat, "filename"));
  }
  else {
    if (!is.data.frame(dat)) {
      stop("Argument 'dat' must be a dataframe or NULL! Class of ",
           "provided argument: ", class(dat));
    }
    res$input$dat.name <- deparse(substitute(dat));
  }

  ### If no variables are specified, take them all.
  if (is.null(x) && is.null(y)) {
    x <- names(dat);
  }

  ### If y was accidently specified, but x wasn't, copy y to x.
  if (is.null(x) && !is.null(y)) {
    x <- y;
  }

  ### Check whether the first vector of variable names has sufficient elements.
  if (length(x) < 1) {
    stop(paste0("Error: x vector has 0 elements or less; ",
                "make sure to specify at least one variable name!."));
  }

  ### Check and store the measurement level of the variables:
  ### dichotomous, nominal, ordinal, or interval
  measurementLevelsX <- vector();
  xCounter <- 1;
  for(curXvar in x) {
    if (var(as.numeric(dat[,curXvar]), na.rm=TRUE) == 0) {
      stop("Variable '", curXvar, "' has no variance (everybody scores the same)! ",
           "This prohibits the calculation of effect size measures, so I'm aborting.");
    }
    if (is.numeric(dat[,curXvar])) {
      measurementLevelsX[xCounter] <- "numeric";
    }
    else if (is.factor(dat[,curXvar])) {
      if (length(levels(dat[,curXvar])) == 2) {
        measurementLevelsX[xCounter] <- "dichotomous";
      }
      else if (is.ordered(dat[,curXvar])) {
        measurementLevelsX[xCounter] <- "ordinal";
      }
      else {
        measurementLevelsX[xCounter] <- "nominal";
      }
    }
    else {
      stop(paste0("Error: variable '", curXvar, "'' does not have ",
                  "nominal, ordinal, or interval measurement level!"));
    }
    xCounter <- xCounter + 1;
  }

  ### Check whether we have a second set of variables
  if (!is.null(y)) {
    ### Check whether the second vector of variable names has sufficient elements.
    if (length(y) < 1) {
      stop(paste0("Error: y vector has 0 elements or less; ",
                  "make sure to specify at least one variable name!."));
    }
    symmetric <- FALSE;
    ### Check and store the measurement level of the variables:
    ### dichotomous, nominal, ordinal, or interval
    measurementLevelsY <- vector();
    yCounter <- 1;
    for(curYvar in y) {
      if (var(as.numeric(dat[,curYvar]), na.rm=TRUE) == 0) {
        stop("Variable '", curYvar, "' has no variance (everybody scores the same)! ",
             "This prohibits the calculation of effect size measures, so I'm aborting.");
      }
      if (is.numeric(dat[,curYvar])) {
        measurementLevelsY[yCounter] <- "numeric";
      }
      else if (is.factor(dat[,curYvar])) {
        if (length(levels(dat[,curYvar])) == 2) {
          measurementLevelsY[yCounter] <- "dichotomous";
        }
        else if (is.ordered(dat[,curYvar])) {
          measurementLevelsY[yCounter] <- "ordinal";
        }
        else {
          measurementLevelsY[yCounter] <- "nominal";
        }
      }
      else {
        stop(paste0("Error: variable '", curYvar, "'' does not have ",
                    "nominal, ordinal, or interval measurement level!"));
      }
      yCounter <- yCounter + 1;
    }
  }
  else {
    symmetric <- TRUE;
    y <- x;
    measurementLevelsY <- measurementLevelsX;
  }

  ### Generate vectors with row and column names
  if (colNames) {
    rowNames <- x;
    columnNames <- y;
  }
  else {
    rowNames <- paste(1:length(x), x, sep=". ");
    columnNames <- paste0(1:length(y), ".");
  }

  ### Generate matrices for results and set row and column names
  res$output$matrix <- list();
  res$output$matrix$es <- matrix(nrow = length(x), ncol = length(y));
  rownames(res$output$matrix$es) <- rowNames;
  colnames(res$output$matrix$es) <- columnNames;
  res$output$matrix$sampleSizes <- matrix(nrow = length(x), ncol = length(y));
  rownames(res$output$matrix$sampleSizes) <- rowNames;
  colnames(res$output$matrix$sampleSizes) <- columnNames;
  res$output$matrix$ci <- matrix(nrow = length(x), ncol = length(y));
  rownames(res$output$matrix$ci) <- rowNames;
  colnames(res$output$matrix$ci) <- columnNames;
  res$output$matrix$full <- matrix(nrow = 2 * length(x), ncol = length(y));
  rownames(res$output$matrix$full) <- rep("", 2*length(rowNames));
  rownames(res$output$matrix$full)[seq(1, (2*length(rowNames)) - 1, by=2)] <- rowNames;
  colnames(res$output$matrix$full) <- columnNames;

  ### Raw results
  res$output$raw <- list();
  res$output$raw$es <- matrix(nrow = length(x), ncol = length(y));
  rownames(res$output$raw$es) <- rowNames;
  colnames(res$output$raw$es) <- columnNames;
  res$output$raw$esType <- matrix(nrow = length(x), ncol = length(y));
  rownames(res$output$raw$esType) <- rowNames;
  colnames(res$output$raw$esType) <- columnNames;
  res$output$raw$ci.lo <- matrix(nrow = length(x), ncol = length(y));
  rownames(res$output$raw$ci.lo) <- rowNames;
  colnames(res$output$raw$ci.lo) <- columnNames;
  res$output$raw$ci.hi <- matrix(nrow = length(x), ncol = length(y));
  rownames(res$output$raw$ci.hi) <- rowNames;
  colnames(res$output$raw$ci.hi) <- columnNames;
  res$output$raw$n <- matrix(nrow = length(x), ncol = length(y));
  rownames(res$output$raw$n) <- rowNames;
  colnames(res$output$raw$n) <- columnNames;
  res$output$raw$p <- matrix(nrow = length(x), ncol = length(y));
  rownames(res$output$raw$p) <- rowNames;
  colnames(res$output$raw$p) <- columnNames;

  xCounter <- 1;
  for(curXvar in x) {
    ### For each row, create the object (list) that will
    ### contain the cells
    res$intermediate$statistics[[curXvar]] <- list();
    res$intermediate$effectSizes[[curXvar]] <- list();
    res$intermediate$sampleSizes[[curXvar]] <- list();
    yCounter <- 1;
    for(curYvar in y) {
      ### If a symmetric table was requested, don't do
      ### anything unless we're in the lower left half.
      if (!symmetric | (yCounter < xCounter)) {

        ### Call the function to compute the statistic.
        ### Which function this is, depends on the preferences
        ### of the user (or the defaults).
        tmpFun <- match.fun(statistic[[measurementLevelsX[xCounter]]]
                                     [[measurementLevelsY[yCounter]]]);
        res$intermediate$statistics[[curXvar]][[curYvar]] <-
          tmpFun(dat[,curXvar], dat[,curYvar], conf.level = conf.level);
        ### We repeat the same trick for the effect sizes.
        tmpFun <- match.fun(effectSize[[measurementLevelsX[xCounter]]]
                            [[measurementLevelsY[yCounter]]]);
        res$intermediate$effectSizes[[curXvar]][[curYvar]] <-
          tmpFun(dat[,curXvar], dat[,curYvar], conf.level = conf.level,
                 var.equal = var.equal);
        res$intermediate$sampleSizes[[curXvar]][[curYvar]] <- nrow(na.omit(dat[,c(curXvar, curYvar)]));
      }
      yCounter <- yCounter + 1;
    }
    xCounter <- xCounter + 1;
  }

  ### Correct p-values for multiple testing
  ### First build a matrix with the raw p-values
  res$intermediate$pvalMatrix <- matrix(nrow=length(x), ncol=length(y), dimnames=list(x, y));
  for(curXvar in x) {
    for(curYvar in y) {
      if (!is.null(res$intermediate$statistics[[curXvar]][[curYvar]]$p.raw)) {
        res$intermediate$pvalMatrix[curXvar, curYvar] <- res$intermediate$statistics[[curXvar]][[curYvar]]$p.raw;
      }
    }
  }
  ### Adjust p-values
  res$intermediate$pvalMatrix.adj <- matrix(p.adjust(res$intermediate$pvalMatrix, method=correction),
                               nrow(res$intermediate$pvalMatrix), ncol(res$intermediate$pvalMatrix),
                               dimnames=dimnames(res$intermediate$pvalMatrix));
  ### Store adjusted p-values in objects
  for(curXvar in x) {
    for(curYvar in y) {
      if (!is.null(res$intermediate$statistics[[curXvar]][[curYvar]]$p.raw)) {
        res$intermediate$statistics[[curXvar]][[curYvar]]$p.adj <-
          res$intermediate$pvalMatrix.adj[curXvar, curYvar];
      }
    }
  }

  ### Run the loop again to create the output matrices (one with point
  ### estimates and p-values corrected for multiple testing; one with
  ### confidence intervals; and one with two rows for each variable,
  ### combining the information).

  for(rowVar in 1:length(x)) {
    for(colVar in 1:length(y)) {
      ### If a symmetric table was requested, only fill the cells if we're
      ### in the lower left half.
      if (!symmetric | (colVar < rowVar)) {
        ### Extract and set confidence interval and then es estimate & p value

        ### Confidence intervals
        res$output$matrix$ci[rowVar, colVar] <- paste0(
          substr(res$intermediate$effectSizes[[rowVar]][[colVar]]$es.type, 1, 1), "=[",
               round(res$intermediate$effectSizes[[rowVar]][[colVar]]$ci[1], digits), "; ",
               round(res$intermediate$effectSizes[[rowVar]][[colVar]]$ci[2], digits), "]");
        res$output$raw$ci.lo[rowVar, colVar] <-
          res$intermediate$effectSizes[[rowVar]][[colVar]]$ci[1];
        res$output$raw$ci.hi[rowVar, colVar] <-
          res$intermediate$effectSizes[[rowVar]][[colVar]]$ci[2];

        ### Effect size
        res$output$matrix$es[rowVar, colVar] <-
          paste0(substr(res$intermediate$effectSizes[[rowVar]][[colVar]]$es.type, 1, 1), "=",
                 round(res$intermediate$effectSizes[[rowVar]][[colVar]]$es, digits), ", ",
                 formatPvalue(res$intermediate$statistics[[rowVar]][[colVar]]$p.adj, digits=pValueDigits, spaces=FALSE));
        res$output$raw$es[rowVar, colVar] <-
          res$intermediate$effectSizes[[rowVar]][[colVar]]$es;
        res$output$raw$esType[rowVar, colVar] <-
          res$intermediate$effectSizes[[rowVar]][[colVar]]$es.type;

        ### P values
        res$output$raw$p[rowVar, colVar] <-
          res$intermediate$statistics[[rowVar]][[colVar]]$p.adj;

        ### Sample sizes
        res$output$matrix$sampleSizes[rowVar, colVar] <-
          res$intermediate$sampleSizes[[rowVar]][[colVar]];
        res$output$raw$n[rowVar, colVar] <-
          res$intermediate$sampleSizes[[rowVar]][[colVar]];

        ### Convert x (row variable) to two row indices in combined matrix
        res$output$matrix$full[(rowVar*2)-1, colVar] <-
          res$output$matrix$ci[rowVar, colVar];
        res$output$matrix$full[(rowVar*2), colVar] <-
          res$output$matrix$es[rowVar, colVar];
        if (((includeSampleSize == "depends") &&
            (length(unique(unlist(res$intermediate$sampleSizes))) > 1)) ||
            (includeSampleSize == "always")) {
          res$output$matrix$full[(rowVar*2), colVar] <-
            paste0(res$output$matrix$full[(rowVar*2), colVar],
                   ", n=", res$output$matrix$sampleSizes[rowVar, colVar]);
        }
      }
      else {
        res$output$matrix$es[rowVar, colVar] <- "";
        res$output$matrix$ci[rowVar, colVar] <- "";
        ### Convert x (row variable) to two row indices in combined matrix
        res$output$matrix$full[(rowVar*2)-1, colVar] <- "";
        res$output$matrix$full[rowVar*2, colVar] <- "";
      }
    }
  }

  ### Set class & return result
  class(res) <- c("associationMatrix");
  return(res);
}

print.associationMatrix <- function (x, type = x$input$type,
                                     info = x$input$info,
                                     file = x$input$file, ...) {

  ### Extract matrix to print (es, ci, or full)
  matrixToPrint <- x$output$matrix[[info[1]]];

  ### Either show in R, or convert to html or latex
  if (toupper(type[1])=="R") {
    if (file=="") {
      print(matrixToPrint, quote=FALSE);
    } else {
      write.table(matrixToPrint, file=file, sep="\t",
                  quote=FALSE, row.names=TRUE, col.names=TRUE);
    }
  }
  else {
    ### Replace even row names (currently empty) with unique comments
    ### for html and LaTeX
    if ((tolower(type[1])=="latex") && (info[1]=='full')) {
      rownames(matrixToPrint)[seq(2, nrow(matrixToPrint), by=2)] <-
        paste0("%%% Variable ", 1:(nrow(matrixToPrint)/2), "\n");
    } else if (info[1]=='full') {
      rownames(matrixToPrint)[seq(2, nrow(matrixToPrint), by=2)] <-
        paste0("<!-- Variable ", 1:(nrow(matrixToPrint)/2), " -->");
    }
    print(xtable(matrixToPrint, align=c('l', rep('c', ncol(matrixToPrint)))),
          type=type,
          html.table.attributes = "cellpadding='5' style='border=0 solid;'",
          sanitize.text.function=function(x) { return (x); },
          file=file);
  }

  invisible();
}

pander.associationMatrix <- function (x,
                                     info = x$input$info,
                                     file = x$input$file, ...) {

  ### Extract matrix to print (es, ci, or full)
  pander(x$output$matrix[[info[1]]],
         missing="");
  invisible();
}
