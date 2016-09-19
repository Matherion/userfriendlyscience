faConfInt <- function(fa) {

  ### Combine both confidence intervals and factor loadings, using
  ### the code from the 'psych:::print.psych.fa.ci' function 
  lc <- data.frame(unclass(fa$loadings), fa$ci$ci);
  ### Create list for CIs per factor
  CIs <- list();
  for (i in 1:fa$factors) {
    CIs[[i]] <- lc[, c(i + fa$factors, i, i + fa$factors * 2)];
    names(CIs[[i]]) <- c('lo', 'est', 'hi');
  }
  
  return(CIs);
  
}
