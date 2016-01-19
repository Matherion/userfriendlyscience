sort.associationMatrix <- function(x, decreasing = TRUE, byColumn = 1, ...) {
  
  ### First generate a numeric table with effectsizes and one with their types
  res <- x;
  res$intermediate$sorting <- list();
  res$intermediate$sorting$esValues <- data.frame();
  res$intermediate$sorting$esTypes <- data.frame();
  
  for (currentRowVar in 1:length(x$intermediate$effectSizes)) {
    for (currentColVar in 1:length(x$intermediate$effectSizes[[currentRowVar]])) {
      res$intermediate$sorting$esValues[currentRowVar, currentColVar] <-
        x$intermediate$effectSizes[[currentRowVar]][[currentColVar]]$es;
      res$intermediate$sorting$esTypes[currentRowVar, currentColVar] <-
        x$intermediate$effectSizes[[currentRowVar]][[currentColVar]]$es.type;
    }
  }
  
  if (length(unique(unlist(res$intermediate$sorting$esTypes))) > 1) {
    warning("The associationMatrix you provided contains ",
            length(unique(unlist(res$intermediate$sorting$esTypes))),
            " different effect size types (specifically ",
            vectTxt(unique(unlist(res$intermediate$sorting$esTypes)), useQuote="'"),
            ")! Sorting may be unreliable.");
  }

  res$intermediate$sorting$order <- order(res$intermediate$sorting$esValues[, byColumn],
                                          decreasing = decreasing);
  res$intermediate$sorting$orderForFull <-
    c(rbind((2*res$intermediate$sorting$order) - 1, 2*res$intermediate$sorting$order));
    
  res$output$matrix.raw <- res$output$matrix;
  
  res$output$matrix$es <- res$output$matrix$es[res$intermediate$sorting$order, ];
  res$output$matrix$ci <- res$output$matrix$ci[res$intermediate$sorting$order, ];
  res$output$matrix$full <- res$output$matrix$full[res$intermediate$sorting$orderForFull, ];

  return(res);
  
}