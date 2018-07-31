#' Visualisation of the power of a Pearson correlation test
#' 
#' This fuction is useful when conducting power analyses for a Pearson
#' correlation. It draws the sampling distribution of Pearson's \emph{r}
#' assuming a null hypothesis value of \emph{r} and assuming a the hypothetical
#' population value. The probability of making a Type 1 error is also
#' illustrated.
#' 
#' 
#' @param n The number of participants.
#' @param rho The value of the correlation coefficient in the population.
#' @param rNull The value of the correlation coefficient according to the null
#' hypothesis.
#' @param distLabels Labels for the two distributions; the first one is the
#' null hypothesis distribution, the second one the alternative distribution.
#' @param rhoColor,rNullColor,type2Color The border colors of the distributions
#' and the region used to illustrate the Type 2 error probability.
#' @param rhoFill,rNullFill,type2Fill The fill colors of the distributions and
#' the region used to illustrate the Type 2 error probability.
#' @param rhoAlpha,rNullAlpha,type2Alpha The alpha (transparency) of the
#' distributions and the region used to illustrate the Type 2 error
#' probability.
#' @param rhoLineSize,rNullLineSize,type2LineSize The line thicknesses of the
#' distributions and the region used to illustrate the Type 2 error
#' probability.
#' @param theme The theme to use.
#' @param alpha The significance level (alpha) of the null hypothesis test.
#' @param digits The number of digits to round to.
#' @return A \code{\link{ggplot}} plot is returned.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{didacticPlot}}
#' @keywords hplot
#' @examples
#' 
#' \dontrun{
#' showPearsonPower();
#' }
#' 
#' @export showPearsonPower
showPearsonPower <- function(n = 100, rho=.3, rNull = 0,
                             distLabels = c("Null Hypothesis", "Population"),
                             rhoColor = 'green', rhoFill = 'green', rhoAlpha = .1, rhoLineSize=1,
                             rNullColor = 'blue', rNullFill = 'blue', rNullAlpha = .1, rNullLineSize = 1,
                             type2Color = 'red', type2Fill = 'red', type2Alpha = .1, type2LineSize = 0,
                             theme = dlvTheme(),
                             alpha = .05,
                             digits=3) {
  
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
  
  dat <- data.frame(pearsonR = pearsonRvalues,
                    rNull = dPearson(pearsonRvalues, N=n, rho=rNull),
                    rho = dPearson(pearsonRvalues, N=n, rho=rho));

  criticalR.lo <- qPearson(alpha/2, rho = rNull, N=n);
  criticalR.hi <- qPearson(1-(alpha/2), rho = rNull, N=n);

  dat$type2error <- ifelse(pearsonRvalues > criticalR.lo &
                             pearsonRvalues < criticalR.hi,
                           dat$rho, 0);
  
  breaks <- round(sort(c(-1, criticalR.lo, criticalR.hi, 0, 1)), digits);
  
  lowerTailPower <- pPearson(criticalR.lo, rho = rho, N=n);
  upperTailPower <- 1 - pPearson(criticalR.hi, rho = rho, N=n);

  power = lowerTailPower + upperTailPower;

  return(ggplot(dat, aes_string(x = 'pearsonR')) +
           
           geom_ribbon(aes_string(ymin=0, ymax='rNull'), color=rNullColor, fill=rNullFill, alpha=rNullAlpha) +
           geom_line(aes_string(y='rNull'), color=rNullColor, size=rNullLineSize) +
           
           geom_ribbon(aes_string(ymin=0, ymax='rho'), color=rhoColor, fill=rhoFill, alpha=rhoAlpha) +
           geom_line(aes_string(y='rho'), color=rhoColor, size=rhoLineSize) +
           
           geom_ribbon(aes_string(ymin=0, ymax='type2error'), color=type2Color, fill=type2Fill, alpha=type2Alpha) +
           geom_line(aes_string(y='type2error'), color=rhoColor, size=type2LineSize) +

           geom_vline(xintercept = criticalR.lo) +
           geom_vline(xintercept = criticalR.hi) +
           
           scale_x_continuous(breaks=breaks) +
           ggtitle(paste0("When N = ", n, " and alpha = ", alpha, ", the power = ", round(power, digits), " against rho = ", rho)) +
           theme); 
         
}

#require('userfriendlyscience');

#require('SuppDists')
#require('ggplot2')
#print(showPearsonPower(n=40));

#print(showPearsonPower(n=40, alpha=.15));

#print(showPearsonPower(n=40, alpha=.45));
