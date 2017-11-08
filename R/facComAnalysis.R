facComAnalysis <- function(data, items=NULL,
                           nfactors = NULL,
                           fm="pa",
                           rotate = "default",
                           covar = FALSE,
                           screeplot = TRUE,
                           SMC = FALSE,
                           maskLoadingsUnder = NULL,
                           showUnrotatedLoadings = FALSE,
                           factorCorrelations = 'score.cor',
                           ...) {

  if (!is.data.frame(data)) {
    stop("Argument 'data' must be a dataframe. You provided '",
         deparse(substitute(data)), "' which has class ",
         vecTxtQ(class(data)), ".");
  }
  if (is.null(items)) {
    items <- names(data);
  } else {
    if (!all(items %in% names(data))) {
      stop("Not all variables you provided in argument ",
           "'items' are present in dataframe '",
           deparse(substitute(data)), "'. Specifically, ",
           vecTxtQ(items[!(items %in% names(data))]),
           ifelse(length(items[!(items %in% names(data))]) == 1, " is", " are"),
           " not.");
    }
  }
  
  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());
  
  res$intermediate$eigen <- data.frame(Factor=1:length(items),
                                       Eigen=eigen(cor(data[, items]))$value);

  if (is.null(nfactors)) {
    res$intermediate$nfactors <- nfactors <-
      sum(res$intermediate$eigen$Eigen >= 1);
  } else if (nfactors > length(items)) {
    stop("You requested ", nfactors,
         " factors, but only provided ", length(items),
         " items.");
  } else {
    res$intermediate$nfactors <- nfactors;
  }
  
  ### Check whether the user wants PCA
  if (fm == 'pca') {
    if (rotate == "default") {
      rotate <- "varimax";
    }
    ### The psych package is a bit . . . Vocal, at times. Write
    ### proper error/warning handlers later.
    theVoid <-
      capture.output(suppressMessages(suppressWarnings(
        res$intermediate$psychObject.rotated <-
          principal(r = data[, items],
                    nfactors = nfactors,
                    rotate = rotate,
                    covar = covar,
                    ...))));
    if (rotate != "none") {
      theVoid <-
        capture.output(suppressMessages(suppressWarnings(
          res$intermediate$psychObject.unrotated <-
            principal(r = data[, items],
                      nfactors = nfactors,
                      rotate = "none",
                      covar = covar,
                      ...))));
    }
  } else {
    if (rotate == "default") {
      rotate <- "oblimin";
    }
    ### The psych package is a bit . . . Vocal, at times. Write
    ### proper error/warning handlers later.
    theVoid <-
      capture.output(suppressMessages(suppressWarnings(
        res$intermediate$psychObject.unrotated <-
          fa(r = data[, items],
             nfactors = nfactors,
             fm = fm,
             rotate = "none",
             SMC = SMC,
             covar = covar,
             ...))));
    
    if (rotate != "none") {
      theVoid <-
        capture.output(suppressMessages(suppressWarnings(
          res$intermediate$psychObject.rotated <-
            fa(r = data[, items],
               nfactors = nfactors,
               fm = fm,
               rotate = rotate,
               SMC = SMC,
               covar = covar,
               ...))));
    }
  }
  
  if ((rotate == 'none')) {
    res$output$loadings <-
      as.matrix(unclass(res$intermediate$psychObject.unrotated$loadings));
    colnames(res$output$loadings) <-
      paste0("Factor ", 1:ncol(res$output$loadings));
  } else {
    res$output$loadings <-
      as.matrix(unclass(res$intermediate$psychObject.rotated$loadings));
    colnames(res$output$loadings) <-
      paste0("Factor ", 1:ncol(res$output$loadings));
    if (showUnrotatedLoadings) {
      res$output$loadings.unrotated <-
        as.matrix(unclass(res$intermediate$psychObject.unrotated$loadings));
      colnames(res$output$loadings.unrotated) <-
        paste0("Factor ", 1:ncol(res$output$loadings.unrotated));
    }
  }

  if (!is.null(res$intermediate$psychObject.rotated[[factorCorrelations]])) {
    res$output$factorCorrelations <- res$intermediate$psychObject.rotated[[factorCorrelations]];
    rownames(res$output$factorCorrelations) <- paste0("Factor ", 1:nrow(res$output$factorCorrelations));
    colnames(res$output$factorCorrelations) <- row.names(res$output$factorCorrelations);
  }

  colnames(res$output$loadings) <-
    paste0("Factor ", 1:ncol(res$output$loadings));
  
  if (!is.null(maskLoadingsUnder) && is.numeric(maskLoadingsUnder)) {
    res$output$loadings[res$output$loadings < maskLoadingsUnder] <-
      NA;
  }

  if (screeplot) {
    res$output$screeplot <-
      ggplot(res$intermediate$eigen,
             aes_string(x = 'Factor',
                        y = 'Eigen')) +
      geom_point() +
      geom_line() +
      theme_minimal() +
      scale_x_continuous(breaks=1:length(items),
                         minor_breaks=NULL) +
      labs(title = 'Scree plot',
           x = 'Factor',
           y = 'Eigen value');
  }
  
  class(res) <- 'facComAnalysis';
  
  return(res);
  
}

print.facComAnalysis <- function(x, digits=2, ...) {
  if (!is.null(x$output$screeplot)) {
    grid.newpage();
    grid.draw(x$output$screeplot);
  }
  
  if (x$input$fm == 'pca') {
    cat0("Principal Components Analysis\n\n");
  } else {
    cat0("Factor Analysis",
         ifelse(x$input$fm == 'pa',
                "",
                paste0("(using method ",
                       x$input$fm,
                       ")")),
         "\n\n");
  }
  
  eigenVector <- x$intermediate$eigen$Eigen;
  names(eigenVector) <- x$intermediate$eigen$Factor;
  cat("Eigen values:\n\n");
  print(eigenVector,digits=digits);
  
  if (!is.null(x$output$loadings.unrotated)) {
    cat("\nFactor loadings before rotation:\n\n");
    print(x$output$loadings.unrotated, digits=digits, na.print="");
  }
  
  cat("\nFactor loadings after rotation:\n\n");
  print(x$output$loadings, digits=digits, na.print="");
  
  if (!is.null(x$output$factorCorrelations)) {  
    cat("\nCorrelations between factors:\n\n");
    print(x$output$factorCorrelations, digits=digits);
  }
  
}
