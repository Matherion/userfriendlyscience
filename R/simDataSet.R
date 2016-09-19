simDataSet <- function(n, varNames,
                       correlations = c(.1, .4),
                       specifiedCorrelations = NULL,
                       means = 0,
                       sds = 1,
                       ranges = c(1, 7),
                       factors = NULL,
                       cuts = NULL,
                       labels = NULL,
                       seed = 20160503,
                       empirical=TRUE,
                       silent = FALSE) {
  
  if (!is.null(seed)) {
    set.seed(seed);
  }
  
  if (length(means) == 1) {
    mu <- rep(means, length(varNames))
  } else {
    mu <- means;
  }
  
  sigma <- matrix(runif(length(varNames) ^ 2,
                        min = min(correlations),
                        max = max(correlations)),
                  ncol=length(varNames));
  
  diag(sigma) <- 1;
  
  rownames(sigma) <- varNames;
  colnames(sigma) <- varNames;
  
  if (!is.null(specifiedCorrelations)) {
    for (index in 1:length(specifiedCorrelations)) {
      sigma[specifiedCorrelations[[index]][1],
            specifiedCorrelations[[index]][2]] <-
        as.numeric(specifiedCorrelations[[index]][3]);
      sigma[specifiedCorrelations[[index]][2],
            specifiedCorrelations[[index]][1]] <-
        as.numeric(specifiedCorrelations[[index]][3]);
    }
  }
  
  if (!silent) {
    cat("Correlation matrix that will be used for the simulation:\n");
    tmp <- sigma;
    rownames(tmp) <- paste0(1:nrow(tmp), ". ", rownames(tmp));
    colnames(tmp) <- paste0(1:ncol(tmp), ". ");
    print(tmp, digits=2);
  }
  
  if (length(sds) == 1) {
    if (length(varNames) == 1) {
      sigma <- sigma * sds;
    }
  } else {
    if (length(sds) != length(varNames)) {
      warning(paste0("If specifying standard deviations, make sure that the 'sds' ",
                     "argument has the same length as the 'varNames' argument! ",
                     "Currently, the 'varNames' argument (", vecTxtQ(varNames),
                     ") has length ", length(varNames), ", but the 'sds' argument (",
                     vecTxt(sds), ") has length ", length(sds), "."));
    } else {
      sigma <- sigma * (sds %o% sds);
    }
  }
  
  if (!silent) {
    cat("\nCovariance matrix that will be used for the simulation:\n");
    tmp <- sigma;
    rownames(tmp) <- paste0(1:nrow(tmp), ". ", rownames(tmp));
    colnames(tmp) <- paste0(1:ncol(tmp), ". ");
    print(tmp, digits=2);
  }
  
  df <- data.frame(mvrnorm(n = n,
                           mu = mu,
                           Sigma = sigma,
                           empirical = empirical));
  
  if (!is.null(factors) && !is.null(cuts) && !is.null(labels)) {
    if (!(length(factors) == length(cuts)) || !(length(cuts) == length(labels))) {
      stop("When specifying factors, make sure the vectors 'factor', 'cuts', and 'labels' ",
           "have the same length!");
    }
    for (currentFactor in 1:length(factors)) {
      if (min(df[, factors[currentFactor]]) < min(cuts[[currentFactor]])) {
        cuts[[currentFactor]] <- c(min(df[, factors[currentFactor]]),
                                   cuts[[currentFactor]]);
      }
      if (max(cuts[[currentFactor]]) < max(df[, factors[currentFactor]])) {
        cuts[[currentFactor]] <- c(cuts[[currentFactor]],
                                   max(df[, factors[currentFactor]]));
      }
      ### Subtract one from lowerbound cut value, and add one to upperbound cut value,
      ### to make sure there are no missing values.
      cuts[[currentFactor]][which(cuts[[currentFactor]] == min(cuts[[currentFactor]]))] <-
        min(cuts[[currentFactor]]) - 1;
      cuts[[currentFactor]][which(cuts[[currentFactor]] == max(cuts[[currentFactor]]))] <-
        max(cuts[[currentFactor]]) + 1;
      
      ### Convert to factor
      df[, factors[currentFactor]] <-
        cut(df[, factors[currentFactor]],
            breaks = cuts[[currentFactor]],
            labels = labels[[currentFactor]]);
    }
  }
  
  ### Transform each variable to the desired range
  if (is.list(ranges)) {
    if (is.null(names(ranges))) {
      for (currentRescaling in 1:length(ranges)) {
        df[, currentRescaling] <- rescale(df[, currentRescaling], range=ranges[[currentRescaling]]);
      }
    } else {
      for (currentRescaling in names(ranges)) {
        df[, currentRescaling] <- rescale(df[, currentRescaling], range=ranges[[currentRescaling]]);
      }
    }
  } else if (length(range) == 2) {
    df[, setdiff(varNames, factors)] <-
      lapply(df[, setdiff(varNames, factors)],
             rescale, range=ranges);
  } else {
    cat("\nInvalid input for 'range' argument (neither a list nor a vector of length 2), ignoring it!\n");
  }
  
  if (!silent) {
    cat("\nCorrelation matrix that was simulated based on this covariance matrix:\n");
    tmp <- cor(massConvertToNumeric(df));
    rownames(tmp) <- paste0(1:nrow(tmp), ". ", rownames(tmp));
    colnames(tmp) <- paste0(1:ncol(tmp), ". ");
    print(tmp, digits=2);
  }
  
  if (!silent) {
    cat("\nSummaries:\n");
    print(summary(df), digits=2);
  }
  
  invisible(as.data.frame(df));
  
}
