showPearsonPower <- function(n = 100, rho=.3, rNull = 0,
                             distLabels = c("Null Hypothesis", "Population"),
                             theme = dlvTheme(),
                             alpha = .05) {
  
  ### Get correlations for which to generate plot
  pearsonRvalues <- seq(from=-1, to=1, by=.001);
  
  ### Get data to plot
  dat <- data.frame(pearsonR = rep(pearsonRvalues, 2),
                                     distribution = factor(rep(c(0, 1), each=2001),
                                                           levels=0:1,
                                                           labels=distLabels));
  dat$density <-
    c(dPearson(pearsonRvalues, N=n, rho=rNull),
      dPearson(pearsonRvalues, N=n, rho=rho));
  
  criticalRlo <- qPearson(alpha/2, n);
  criticalRhi <- qPearson(1-(alpha/2), n);
  
  breaks <- round(sort(c(-1, criticalRlo, criticalRhi, 0, 1)), 3);
  
  return(ggplot(dat, aes(x = pearsonR,
                         y = density,
                         group = distribution,
                         color = distribution)) +
           geom_vline(xintercept = criticalRlo) +
           geom_vline(xintercept = criticalRhi) +
           geom_line() +
           scale_x_continuous(breaks=breaks) +
           theme);
         
}

print(showPearsonPower());
