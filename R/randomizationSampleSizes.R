randomizationSampleSizes <- function(nVars,
                                     pSuccess = .95,
                                     sampleSizes = seq(from=20, to=1000, by=20),
                                     d = .2,
                                     nClusters = 1,
                                     rMean = 0,
                                     samples = 1000,
                                     progressBar = progress_text(style=3),
                                     pbLabel = paste0("Running simulations for ", nClusters,
                                              " clusters of confounders, with a mean intracluster ",
                                              "correlation of ", rMean, ".\n")) {
  
  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());
  
  cat0(pbLabel);
  
  simulatedCohensDs <-
    raply(.n = samples,
          .expr = sapply(sampleSizes, randomizationSampleSizesSample,
                         nVars = nVars, nClusters = nClusters,
                         rMean = rMean),
          .progress = progressBar);
  
  ### Using the thresholds specified in the criticalCohensDs to
  ### establish how often a confounder differs between the conditions,
  ### separate for each sample size.
  if (nVars == 1) {
    ### With only one variable, we only have to count the proportion
    ### of replicates with group differences larger than the specified
    ### Cohen's d values.
    proportionsFailedRandomizations <- t(apply(simulatedCohensDs, 2, function(x) {
      ### Count '1' for all replicates where any of the variables is
      ### significant and '0' when none is using 'any' and 'apply', then
      ### count how many replicates have at least one confounder that
      ### differs between the groups using 'mean'.
      return(sapply(d, function(d) {
        return(mean(abs(x) > d));
      }));
    }));
  } else {
    proportionsFailedRandomizations <- t(apply(simulatedCohensDs, 3, function(x) {
      ### Count '1' for all replicates where any of the variables is
      ### significant and '0' when none is using 'any' and 'apply', then
      ### count how many replicates have at least one confounder that
      ### differs between the groups using 'mean'.
      return(sapply(d, function(d) {
        return(mean(apply(abs(x) > d, 1, any)));
      }));
    }));
  }
  
  ### Set variable (column) names
  if (length(d) == 1) {
    proportionsFailedRandomizations <- t(proportionsFailedRandomizations);
  }
  
  ### Add column with sample sizes
  proportionsFailedRandomizations <- data.frame(sampleSizes,
                                                proportionsFailedRandomizations);
  
  names(proportionsFailedRandomizations) <- c('n', paste0("d=", d));
  
  ggplotYvar <- names(proportionsFailedRandomizations)[2];
  
  res$intermediate$dat <- proportionsFailedRandomizations;
  
  res$output$requiredN <- min(proportionsFailedRandomizations[proportionsFailedRandomizations[, ggplotYvar] <
                                                                (1 - pSuccess), 'n']);
  
  res$output$plot <- qplot(x = proportionsFailedRandomizations[, 'n'],
                           y = proportionsFailedRandomizations[, ggplotYvar]) +
    geom_line(size=1, color='black') +
    geom_hline(yintercept = 1 - pSuccess) +
    geom_vline(xintercept = res$output$requiredN) +
    scale_y_continuous(limits=c(0, 1)) +
    xlab("Sample size (n)") +
    ylab("Probability of at least one confounder") +
    ggtitle(paste0("Probability of confounding for ", nVars, " nuisance variables")) +
    theme_bw();
  
  class(res) <- 'randomizationSampleSizes';
  
  return(res);
  
}

print.randomizationSampleSizes <- function(x, ...) {
  print(x$output$plot, ...);
  cat0("With ", x$input$nVars, " nuisance variable(s) in ", x$input$nClusters,
       " cluster(s) with a mean intra-cluster correlation of r=", x$input$rMean,
       ", to have a probability of ",
       100 * x$input$pSuccess, "% to have two groups differ at most with ",
       "a Cohen's d=", x$input$d, " for one or more nuisance variables, ",
       "on the basis of ", x$input$samples, " simulated replicates, at least a ",
       "sample size of n=", x$output$requiredN, " is required.");
  invisible();
}
