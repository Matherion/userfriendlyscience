### Based on http://sas-and-r.blogspot.nl/2011/06/example-839-calculating-cramers-v.html
### (i.e. see the comments by Nick Horton and thelatemail)

### Function to compute Cramer's V
cramersV <- function(x, y = NULL, digits=2) {
  
  res <- list(input = list(x=x, y=y, digits=digits),
              intermediate = list(),
              output = list(),
              error = list());
  
  if (is.null(y)) {
    if (!is.table(x) && !is.matrix(x)) {
      stop("If argument 'y' is empty, argument 'x' must be a matrix or a ",
           "table! Instead, it has class ", class(x), ".");
    } else {
      ### This catches the chisquare warning when the approximation may be
      ### incorrect
      suppressWarnings(withCallingHandlers({
        res$intermediate$chisq.test <- chisq.test(x, correct=FALSE);
      }, warning = function(w) {
        if (grepl("Chi-squared approximation may be incorrect", w)) {
          res$errors[[length(res$errors) + 1]] <- w;
        } else {
          warning(w);
        }
      }));
      res$intermediate$n <- sum(x);
      res$intermediate$leastCols <- min(nrow(x), ncol(x));
    }
  }
  else {
    if (length(x) != length(y)) {
      stop("The length of arguments 'x' and 'y' is not the same; are you ",
           "sure they're both vectors of equal length?");
    } else {
      ### This catches the chisquare warning when the approximation may be
      ### incorrect
      suppressWarnings(withCallingHandlers({
        res$intermediate$chisq.test <- chisq.test(x, y, correct=FALSE);
      }, warning = function(w) {
        if (grepl("Chi-squared approximation may be incorrect", w)) {
          res$errors[[length(res$errors) + 1]] <- w;
        } else {
          warning(w);
        }
      }));
      res$intermediate$n <- length(x);
      res$intermediate$leastCols <- min(length(unique(x)), length(unique(y)));
    }
  }
  
  res$output$cramersV <- as.numeric(sqrt(res$intermediate$chisq.test$statistic /
                         (res$intermediate$n * (res$intermediate$leastCols - 1))));
  
  class(res) <- 'CramersV';
  return(res);
}

print.CramersV <- function(x, digits=x$input$digits, ...) {
  cat(paste0("Cram\u00E9r's V = "),
      signif(x$output$cramersV, digits=digits));
}



#' crossTab, confIntV and cramersV
#' 
#' These functions compute the point estimate and confidence interval for
#' Cramer's V. The crossTab function also shows a crosstable.
#' 
#' 
#' @aliases confIntV cramersV crossTab
#' @param x Either a crosstable to analyse, or one of two vectors to use to
#' generate that crosstable. The vector should be a factor, i.e. a categorical
#' variable identified as such by the 'factor' class).
#' @param y If x is a crosstable, y can (and should) be empty. If x is a
#' vector, y must also be a vector.
#' @param digits Minimum number of digits after the decimal point to show in
#' the result.
#' @param pValueDigits Minimum number of digits after the decimal point to show
#' in the Chi Square p value in the result.
#' @param conf.level Level of confidence for the confidence interval.
#' @param samples Number of samples to generate when bootstrapping.
#' @param method Whether to use Fisher's Z or bootstrapping to compute the
#' confidence interval.
#' @param storeBootstrappingData Whether to store (or discard) the data
#' generating during the bootstrapping procedure.
#' @param ...  Extra arguments to \code{crossTab} are passed on to
#' \code{confIntV}.
#' @return
#' 
#' The cramersV and confIntV functions return either a point estimate or a
#' confidence interval for Cramer's V, an effect size to describe the
#' association between two categorical variables. The crossTab function is just
#' a wrapper around confIntV.
#' @keywords bivar
#' @examples
#' 
#' 
#' crossTab(infert$education, infert$induced, samples=50);
#' 
#' ### Get confidence interval for Cramer's V
#' ### Note that by using 'table', and so removing the raw data, inhibits
#' ### bootstrapping, which could otherwise take a while.
#' confIntV(table(infert$education, infert$induced));
#' 
#' 
#' @export confIntV
confIntV <- function(x, y = NULL, conf.level=.95,
                     samples = 500, digits=2,
                     method=c('bootstrap', 'fisher'),
                     storeBootstrappingData = FALSE) {
  
  res <- list(input = as.list(environment()),
              intermediate = list(ps = c((1-conf.level)/2,
                                         1-((1-conf.level)/2))),
              output = list());
  
  if (is.null(y)) {
    if (!is.table(x) && !is.matrix(x)) {
      stop("If argument 'y' is empty, argument 'x' must be a matrix or a ",
           "table! Instead, it has class ", class(x), ".");
    } else {
      res$intermediate$n <- sum(x);
      res$intermediate$table <- x;
    }
  } else {
    if (length(x) != length(y)) {
      stop("The length of arguments 'x' and 'y' is not the same; are you ",
           "sure they're both vectors of equal length?");
    }
    
    res$intermediate$table <- table(x, y);
    
    res$intermediate$varNames <- c(deparse(substitute(x)), deparse(substitute(y)));

    if ("bootstrap" %in% method) {
      
      res$intermediate$dat <- data.frame(x=res$input$x, y=res$input$y);

      bootstrapFull <- unlist(lapply(1:samples,
                              function(i, fullDat = res$intermediate$dat) {
                                dat <- fullDat[sample(seq(1, nrow(fullDat)),
                                                      replace=TRUE), ];
                                return(cramersV(dat$x,
                                                dat$y)$output$cramersV);
                              }));

      res$output$confIntV.bootstrap <-
        quantile(bootstrapFull, res$intermediate$ps);
      
      if (storeBootstrappingData) {
        res$intermediate$bootstrapFull <- bootstrapFull;
      }
      
    }
    
  }
  
  ### Store point estimate
  res$intermediate$cramersV <- cramersV(x=x, y=y);
  
  if ("fisher" %in% method) {
    
    # convert the Cramer's V to a Fisher's Z
    res$intermediate$fisherZ <- 0.5 * log((1 + res$intermediate$cramersV$output$cramersV)/
                                            (1 - res$intermediate$cramersV$output$cramersV));
    
    # calculate 95% conf.int around Fisher's Z
    res$intermediate$fisherZ.se <-
      1/sqrt(sum(res$intermediate$table)-3) * qnorm(res$intermediate$ps[2]);
    
    res$intermediate$fisherZ.ci <- res$intermediate$fisherZ +
      c(-res$intermediate$fisherZ.se, res$intermediate$fisherZ.se)
    
    # convert it back to conf.int around Cramer's V
    res$output$confIntV.fisher <- (exp(2 * res$intermediate$fisherZ.ci) - 1) /
      (1 + exp(2 * res$intermediate$fisherZ.ci));
  }

  ### Correct impossible values
  res$output$confIntV.fisher[res$output$confIntV.fisher < 0] <- 0;
  res$output$confIntV.bootstrap[res$output$confIntV.bootstrap < 0] <- 0;
  
  class(res) <- 'confIntV';
  return(res);    
}

print.confIntV <- function(x, digits=x$input$digits, ...) {
  cat(paste0("Cram\u00E9r's V ", 100*x$input$conf.level,
             "% confidence interval (point estimate = ",
             formatR(x$intermediate$cramersV$output$cramersV, digits=digits),
             "):  \n"));
  if (!is.null(x$input$y) && "bootstrap" %in% x$input$method) {
    cat(paste0("Bootstrapped: ",
               formatCI(x$output$confIntV.bootstrap, digits=digits,
                        noZero=TRUE),
               "  \n"));
  }
  if ("fisher" %in% x$input$method) {
    cat(paste0("Using Fisher's z: ",
               formatCI(x$output$confIntV.fisher, digits=digits,
                        noZero=TRUE),
               "\n"));
  }
}
