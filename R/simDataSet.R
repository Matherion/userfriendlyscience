#' simDataSet
#' 
#' simDataSet can be used to conveniently and quickly simulate a dataset that
#' satisfies certain constraints, such as a specific correlation structure,
#' means, ranges of the items, and measurement levels of the variables. Note
#' that the results are approximate; mvrnorm is used to generate the
#' correlation matrix, but the factor are only created after that, so cutting
#' the variable into factors may change the correlations a bit.
#' 
#' This function was intended to allow relatively quick generation of datasets
#' that satisfy specific constraints, e.g. including a number of factors,
#' variables with a specified minimum and maximum value or specified means and
#' standard deviations, and of course specific correlations. Because all
#' correlations except those specified are randomly generated from a uniform
#' distribution, it's quite convenient to generate messy kind of real looking
#' datasets quickly. Note that it's mostly a convenience function, and datasets
#' will still require tweaking; for example, factors are simply numeric vectors
#' that are \code{\link{cut}} *after* \code{\link{mvrnorm}} generated the data,
#' so the associations will change slightly.
#' 
#' @param n Number of requires cases (records, entries, participants, rows) in
#' the final dataset.
#' @param varNames Names of the variables in a vector; note that the length of
#' this vector will determine the number of variables simulated.
#' @param correlations The correlations between the variables are randomly
#' sampled from this range using the uniform distribution; this way, it's easy
#' to have a relatively 'messy' correlation matrix without the need to specify
#' every correlation manually.
#' @param specifiedCorrelations The correlations that have to have a specific
#' value can be specified here, as a list of vectors, where each vector's first
#' two elements specify variables names, and the last one the correlation
#' between those two variables. Note that tweaking the correlations may take
#' some time; the \code{\link{mvrnorm}} function will complain that "'Sigma' is
#' not positive definite", or in other words, you supplied a combination of
#' correlations that can't exist simultaneously, if you get it wrong.
#' @param means,sds The means and standard deviations of the variables. Note
#' that is you set \code{ranges} for one or more variables (see below), those
#' ranges are used to rescale those variables, overriding any specified means
#' and standard deviations. If only one mean or standard deviation is supplied,
#' it's recycled along the variables.
#' @param ranges The desired ranges of the variables, supplied as a named list
#' where the name of each element corresponds to a variable. The
#' \code{\link{rescale}} function will be used to rescale those variables for
#' which a desired scale is specified here. Note that for those variables, the
#' means and standard deviations will be determined by these new ranges.
#' @param factors A vector of variable names that should be converted into
#' factors (using \code{\link{cut}}). Make sure to specify lists for
#' \code{cuts} and \code{labels} as well (of the same length).
#' @param cuts A list of vectors that specify, for each factor, where to 'cut'
#' the numeric vector into factor levels.
#' @param labels A list of vectors that specify, for each factor, and for each
#' level, the labels that should be assigned to the factor levels. Each vector
#' in this list has to have one more element than each vector in the
#' \code{cuts} list.
#' @param seed The seed to use when generating the dataset (to make sure the
#' exact same dataset can be generated repeatedly).
#' @param empirical Whether to generate the data using the exact
#' (\code{empirical = TRUE}) or approximate (\code{empirical = FALSE})
#' correlation matrix; this is passed on to \code{\link{mvrnorm}}.
#' @param silent Whether to show intermediate and final descriptive information
#' (correlation and covariance matrices as well as summaries).
#' @return The generated dataframe is returned invisibly.
#' @author Gjalt-Jorn Peters
#' @seealso \code{\link{mvrnorm}}
#' @keywords utilities
#' @examples
#' 
#' dat <- simDataSet(500, varNames=c('age',
#'                                   'sex',
#'                                   'educationLevel',
#'                                   'negativeLifeEventsInPast10Years',
#'                                   'problemCoping',
#'                                   'emotionCoping',
#'                                   'resilience',
#'                                   'depression'),
#'                   means = c(40,
#'                             0,
#'                             0,
#'                             5,
#'                             3.5,
#'                             3.5,
#'                             3.5,
#'                             3.5),
#'                   sds = c(10,
#'                           1,
#'                           1,
#'                           1.5,
#'                           1.5,
#'                           1.5,
#'                           1.5,
#'                           1.5),
#'                   specifiedCorrelations =
#'                     list(c('problemCoping', 'emotionCoping', -.5),
#'                          c('problemCoping', 'resilience', .5),
#'                          c('problemCoping', 'depression', -.4),
#'                          c('depression', 'emotionCoping', .6),
#'                          c('depression', 'resilience', -.3)),
#'                   ranges = list(age = c(18, 54),
#'                                 negativeLifeEventsInPast10Years = c(0,8),
#'                                 problemCoping = c(1, 7),
#'                                 emotionCoping = c(1, 7)),
#'                   factors=c("sex", "educationLevel"),
#'                   cuts=list(c(0),
#'                             c(-.5, .5)),
#'                   labels=list(c('female', 'male'),
#'                               c('lower', 'middle', 'higher')),
#'                   silent=FALSE);
#' 
#' @export simDataSet
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
        df[, currentRescaling] <- scales::rescale(df[, currentRescaling], to=ranges[[currentRescaling]]);
      }
    } else {
      for (currentRescaling in names(ranges)) {
        df[, currentRescaling] <- scales::rescale(df[, currentRescaling], to=ranges[[currentRescaling]]);
      }
    }
  } else if (length(ranges) == 2) {
    fromRange <- c(min(sapply(df[, setdiff(varNames, factors)], min)),
                   max(sapply(df[, setdiff(varNames, factors)], max)));
    if (!silent) {
      cat0("Rescaling all variables from ",
           vecTxt(round(fromRange, 2)), " to ",
           vecTxt(ranges), ".\n");
    }
    df[, setdiff(varNames, factors)] <-
      lapply(df[, setdiff(varNames, factors)],
             scales::rescale, to=ranges, from=fromRange);
  } else {
    cat("\nInvalid input for 'ranges' argument (neither a list nor a vector of length 2), ignoring it!\n");
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
