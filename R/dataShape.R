dataShape <- function(sampleVector, na.rm=TRUE, type=2, digits=2,
                      conf.level=.95, plots=TRUE, xLabs = NA,
                      yLabs = NA, qqCI=TRUE, labelOutliers = TRUE,
                      sampleSizeOverride = NULL) {
  
  ### Note: this is adapted from the 'skewness' and 'kurtosis' functions
  ### in 'e1071' and the way they're computed in 'describe' in 'psych', and
  ### for the variances, also based on Stan Browns page at
  ### http://www.tc3.edu/instruct/sbrown/stat/shape.htm, as well as of course
  ### on Joanes & Gill (1998)
  
  res <- list(input = as.list(environment()),
              intermediate = list(), output = list());
  
  if (any(isNA <- is.na(sampleVector))) {
    if (na.rm) {
      res$intermediate$sampleVector <- sampleVector <- sampleVector[!isNA];
    }
    else {
      return(NA);
    }
  } else {
    res$intermediate$sampleVector <- sampleVector;
  }
  
  if (!(type %in% (1:3))) {
    stop("Invalid 'type' argument. This must be 1, 2 or 3; see ?skewness for more info.");
  }
  
  ### Number of observations
  res$intermediate$n <- n <- length(sampleVector);
  
  if (n < 3) {
    stop("There are only ", n, " datapoints/observations - this is too few to ",
         "compute a sensible estimate of the 'shape' of this data.");
  }
  
  ### First moment (mean)
  res$intermediate$m1 <-       sum(sampleVector^1) / n;

  ### Center the data
  res$intermediate$centeredVector <- sampleVector <- sampleVector - res$intermediate$m1;

  ### Second moment (variance)
  res$intermediate$m2 <- m2 <- sum(sampleVector^2) / n;
  
  ### Third moment
  res$intermediate$m3 <- m3 <- sum(sampleVector^3) / n;
  
  ### Fourth moment
  res$intermediate$m4 <- m4 <- sum(sampleVector^4) / n;

  ### Sample skewness
  res$intermediate$g1 <- m3 / (m2 ^ (3/2));

  ### Sample (excess) kurtosis
  res$intermediate$g2 <- (m4 / (m2 ^ 2)) - 3;
  
  ###  Population skewness
  res$intermediate$G1 <- res$intermediate$g1 *
                         sqrt( n * (n-1) ) /
                         ( n - 2 );

  ### Population kurtosis
  res$intermediate$G2 <- (n-1) *
                         ( (n+1) * res$intermediate$g2 + 6 ) /
                         ( (n-2) * (n-3) );
  
  ### Type 3 correction for skewness
  res$intermediate$b1 <- res$intermediate$g1 *
                         (((n-1) / n) ^ (3/2));
  
  ### Type 3 correction for kurtosis
  res$intermediate$b2 <- (res$intermediate$g2 + 3) *
                         (((n-1) / n) ^ 2) - 3;
  
  ### Now for the variances. We compute the variance in a normal
  ### population, because when we want to test the skewness and
  ### kurtosis, we do so under the assumption that they come from a
  ### normal distribution. Plus, no idea how to compute the variance
  ### otherwise :-)
  
  ### These formulas come from the Joanes & Gill (1998) article.
  
  ### Note that we use the 'overridden sample size' if one was specified.
  ### We do this because when this function is used 
  if (is.numeric(sampleSizeOverride)) n <- sampleSizeOverride;
  
  res$intermediate$var.g1 <- ( 6 * (n-2) ) /
                             ( (n+1) * (n+3) );
  res$intermediate$var.g2 <- ( 24*n * (n-2) * (n-3) ) /
                             ( (n+1)^2 * (n+3) * (n + 5) );
  res$intermediate$var.G1 <- res$intermediate$var.g1 *
                             sqrt(n * (n-1)) /
                             (n-2)^2;
  res$intermediate$var.G2 <- res$intermediate$var.g2 *
                             (((n-1) * (n+1)) /
                              ((n-2) * (n-3))) ^ 2;
  res$intermediate$var.b1 <- res$intermediate$var.g1 *
                             ((n-1) / n) ^ 3;
  res$intermediate$var.b2 <- res$intermediate$var.g2 *
                             ((n-1) / n) ^ 4;
  
  ### And the standard error for the skewness and the kurtosis,
  ### based on the formulas on http://www.tc3.edu/instruct/sbrown/stat/shape.htm
  res$intermediate$se.G1 <- sqrt( ( 6 * n * (n-1) ) /
                                  ( (n-2) * (n+1) * (n+3) ) );
  res$intermediate$se.G2 <- 2 * res$intermediate$se.G1 *
                            sqrt( (n^2 - 1) /
                                  ( (n-3) * (n+5) ) );
  
  ### Confidence interval for skewness and kurtosis in the population
  res$intermediate$zMultiplier <- qnorm(1 - (1-conf.level)/2);
  res$intermediate$ci.G1 <- c(res$intermediate$G1 -
                                res$intermediate$zMultiplier *
                                res$intermediate$se.G1,
                              res$intermediate$G1 +
                                res$intermediate$zMultiplier *
                                res$intermediate$se.G1);
  res$intermediate$ci.G2 <- c(res$intermediate$G2 -
                                res$intermediate$zMultiplier *
                                res$intermediate$se.G2,
                              res$intermediate$G2 +
                                res$intermediate$zMultiplier *
                                res$intermediate$se.G2);
  
  ### Z-test to test against normal distribution
  res$intermediate$z.G1 <- res$intermediate$G1 / res$intermediate$se.G1;
  res$intermediate$z.G2 <- res$intermediate$G2 / res$intermediate$se.G2;
  res$intermediate$p.G1 <- 2*(1-pnorm(abs(res$intermediate$z.G1)));
  res$intermediate$p.G2 <- 2*(1-pnorm(abs(res$intermediate$z.G2)));
  
  ### Hartigans' Dip Test
  res$output$D.dip.test <- dip.test(sampleVector)$statistic[[1]];
  res$output$p.dip.test <- dip.test(sampleVector)$p[[1]];
  
  ### Store requested estimates in output object
  if (type == 1) {
    res$output$skewness <- res$intermediate$g1;
    res$output$kurtosis <- res$intermediate$g2;
    res$output$type <- "g";
  }
  else if (type == 2) {
    res$output$skewness <- res$intermediate$G1;
    res$output$kurtosis <- res$intermediate$G2;
    res$output$type <- "G";
  }
  else if (type == 3)  {
    res$output$skewness <- res$intermediate$b1;
    res$output$kurtosis <- res$intermediate$b2;
    res$output$type <- "b";
  }
  
  if (plots) {
    res$intermediate$histogram <-
      powerHist(res$intermediate$sampleVector)$plot;
    res$intermediate$qq <-
      ggqq(res$intermediate$sampleVector, ci=qqCI);
    res$intermediate$boxplot <-
      ggBoxplot(res$intermediate$sampleVector, labelOutliers=labelOutliers);
    if (!is.null(xLabs)) {
      if (is.na(xLabs)) {
        res$intermediate$histogram <-
          res$intermediate$histogram +
          theme(axis.title.x = element_blank());
        res$intermediate$qq <-
          res$intermediate$qq +
          theme(axis.title.x = element_blank());
        res$intermediate$boxplot <-
          res$intermediate$boxplot +
          theme(axis.title.x = element_blank());
      } else {
        res$intermediate$histogram <-
          res$intermediate$histogram + xlab(xLabs$hist);
        res$intermediate$qq <-
          res$intermediate$qq + xlab(xLabs$qq);
        res$intermediate$boxplot <-
          res$intermediate$boxplot + xlab(xLabs$box);
      }
    }

    if (!is.null(yLabs)) {
      if (is.na(yLabs)) {
        res$intermediate$histogram <-
          res$intermediate$histogram +
          theme(axis.title.y = element_blank());
        res$intermediate$qq <-
          res$intermediate$qq +
          theme(axis.title.y = element_blank());
        res$intermediate$boxplot <-
          res$intermediate$boxplot +
          theme(axis.title.y = element_blank());
      } else {
        res$intermediate$histogram <-
          res$intermediate$histogram + ylab(yLabs$hist);
        res$intermediate$qq <-
          res$intermediate$qq + ylab(yLabs$qq);
        res$intermediate$boxplot <-
          res$intermediate$boxplot + ylab(yLabs$box);
      }
    }
    
    res$output$plot <- arrangeGrob(res$intermediate$histogram,
                                   res$intermediate$qq,
                                   res$intermediate$boxplot, ncol=3);
  }
  
  ### Return results object
  class(res) <- "dataShape";
  return(res);
  
}

print.dataShape <- function(x, digits=x$input$digits, extraNotification=TRUE, ...) {
  if (x$output$type == "G") {
    skewness.inference <- paste0("  (se = ", format(x$intermediate$se.G1, digits=digits),
                                 ", confidence interval = [",
                                 paste0(format(x$intermediate$ci.G1, digits=digits), collapse=", "),
                                 "], z = ", format(x$intermediate$z.G1, digits=digits),
                                 ", ", formatPvalue(x$intermediate$p.G1, digits=digits), ")");
    kurtosis.inference <- paste0("  (se = ", format(x$intermediate$se.G2, digits=digits),
                                 ", confidence interval = [",
                                 paste0(format(x$intermediate$ci.G2, digits=digits), collapse=", "),
                                 "], z = ", format(x$intermediate$z.G2, digits=digits),
                                 ", ", formatPvalue(x$intermediate$p.G2, digits=digits), ")");
  }
  else {
    skewness.inference <- kurtosis.inference <- "";
  }
  cat(paste0("Skewness (", x$output$type, "1): ",
             round(x$output$skewness, digits=digits),
             skewness.inference, "\n"));
  cat(paste0("Kurtosis (", x$output$type, "2): ",
             round(x$output$kurtosis, digits=digits),
             kurtosis.inference, "\n"));
  cat(paste0("Hartigans' Dip Test: ",
             round(x$output$D.dip.test, digits=digits), ", ",
             formatPvalue(x$output$p.dip.test), "\n"));
  
  if (x$input$plots) {
    grid.draw(x$output$plot);
  }  
  
  if (extraNotification && x$output$type == "g") {
    cat("\nNote: g1 and g2 are biased estimates of the population skewness and kurtosis.",
        "For unbiased estimates, use 'type=2' or 'type=3'.");
  }
  else if (extraNotification && x$output$type == "G") {
    cat("\nNote: G1 and G2 are the estimates for skewness and kurtosis used by SPSS and SAS,",
        "and corrected for the bias present in g1 and g2 ('type=1'). Note that b1 and b2 ('type=3')",
        "may perform better in small samples from a normal distribution.");
  }
  else if (extraNotification && x$output$type == "b") {
    cat("\nNote: b1 and b2 are estimates for skewness and kurtosis that have smaller mean-squared error in small",
        "samples from a normal distribution than G1 and G2, which are used by SPSS and SAS ('type=2').");
  }
}

pander.dataShape <- function(x, digits=x$input$digits, extraNotification=TRUE, ...) {
  if (x$output$type == "G") {
    skewness.inference <- paste0("  (se = ", format(x$intermediate$se.G1, digits=digits),
                                 ", confidence interval = [",
                                 paste0(format(x$intermediate$ci.G1, digits=digits), collapse=", "),
                                 "], z = ", format(x$intermediate$z.G1, digits=digits),
                                 ", ", formatPvalue(x$intermediate$p.G1, digits=digits), ")");
    kurtosis.inference <- paste0("  (se = ", format(x$intermediate$se.G2, digits=digits),
                                 ", confidence interval = [",
                                 paste0(format(x$intermediate$ci.G2, digits=digits), collapse=", "),
                                 "], z = ", format(x$intermediate$z.G2, digits=digits),
                                 ", ", formatPvalue(x$intermediate$p.G2, digits=digits), ")");
  }
  else {
    skewness.inference <- kurtosis.inference <- "";
  }
  cat(paste0("Skewness (", x$output$type, "1): ",
             round(x$output$skewness, digits=digits),
             skewness.inference, "  \n"));
  cat(paste0("Kurtosis (", x$output$type, "2): ",
             round(x$output$kurtosis, digits=digits),
             kurtosis.inference, "  \n"));
  cat(paste0("Hartigans' Dip Test: ",
             round(x$output$D.dip.test, digits=digits), ", ",
             formatPvalue(x$output$p.dip.test), "\n"));
  
  if (extraNotification && x$output$type == "g") {
    cat("\nNote: g1 and g2 are biased estimates of the population skewness and kurtosis.",
        "For unbiased estimates, use 'type=2' or 'type=3'.\n");
  }
  else if (extraNotification && x$output$type == "G") {
    cat("\nNote: G1 and G2 are the estimates for skewness and kurtosis used by SPSS and SAS,",
        "and corrected for the bias present in g1 and g2 ('type=1'). Note that b1 and b2 ('type=3')",
        "may perform better in small samples from a normal distribution.\n");
  }
  else if (extraNotification && x$output$type == "b") {
    cat("\nNote: b1 and b2 are estimates for skewness and kurtosis that have smaller mean-squared error in small",
        "samples from a normal distribution than G1 and G2, which are used by SPSS and SAS ('type=2').\n");
  }
  
  cat("\n");
  
  if (x$input$plots) {
    grid.draw(x$output$plot);
  }  
  
}
