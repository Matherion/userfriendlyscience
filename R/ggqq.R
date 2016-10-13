### This function is based on the function by GitHub user Rentrop (https://github.com/rentrop),
### taken from the gist at https://gist.github.com/rentrop/d39a8406ad8af2a1066c,
### which itself uses code from 'car:::qqPlot'.

ggqq <- function(x, distribution = "norm", ...,
                  ci = TRUE,
                  line.estimate = NULL,
                  conf.level = 0.95,
                  xlab = "Theoretical quantiles",
                  ylab = "Observed quantiles",
                  theme = theme_bw()){
  
  q.function <- eval(parse(text = paste0("q", distribution)));
  d.function <- eval(parse(text = paste0("d", distribution)));
  x <- na.omit(x);
  ord <- order(x);
  n <- length(x);
  P <- ppoints(length(x));
  df <- data.frame(ord.x = x[ord], z = q.function(P, ...));
  
  if(is.null(line.estimate)){
    Q.x <- quantile(df$ord.x, c(0.25, 0.75));
    Q.z <- q.function(c(0.25, 0.75), ...);
    b <- diff(Q.x)/diff(Q.z);
    coef <- c(Q.x[1] - b * Q.z[1], b);
  } else {
    coef <- coef(line.estimate(ord.x ~ z));
  }
  
  zz <- qnorm(1 - (1 - conf.level)/2);
  SE <- (coef[2]/d.function(df$z)) * sqrt(P * (1 - P)/n);
  fit.value <- coef[1] + coef[2] * df$z;
  df$upper <- fit.value + zz * SE;
  df$lower <- fit.value - zz * SE;

  p <- ggplot(df, aes(x=z, y=ord.x)) +
    geom_point() + 
    geom_abline(intercept = coef[1], slope = coef[2]) +
    xlab(xlab) + ylab(ylab) + theme;

  if (ci) {
    p <- p +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2);
  }
  
  return(p);
}
