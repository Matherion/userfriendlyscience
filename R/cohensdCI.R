cohensdCI <- function(d, n, conf.level = .95, plot=FALSE, silent=TRUE) {
  
  if (length(conf.level) != 1) {
    stop("Only specify one value for argument 'conf.level'!");
  }
  
  ci.bound.lo <- (1 - conf.level) / 2;
  ci.bound.hi <- 1 - (1 - conf.level) / 2;
  
  ### From a post at the R-help mailig list by Luke Tierney, see
  ### http://stackoverflow.com/questions/3903157/how-can-i-check-whether-a-function-call-results-in-a-warning
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w));
    invokeRestart("muffleWarning");
  }
  myWarnings <- NULL;

  if (length(d) == length(n)) {
    res <- t(sapply(1:length(d), function(i) {
    return(withCallingHandlers(c(qCohensd(ci.bound.lo, n[i], populationD=d[i]),
                                 qCohensd(ci.bound.hi, n[i], populationD=d[i])),
                               warning = wHandler));
    }));
  } else if ((length(d) == 1) || (length(n) == 1)) {
    res <- withCallingHandlers(matrix(c(qCohensd(ci.bound.lo, n, populationD=d),
                                        qCohensd(ci.bound.hi, n, populationD=d)), ncol=2),
                               warning = wHandler);
  } else {
    stop("Either specify vectors of equal length as 'd' and 'n', or a ",
         "single value for one and a vector for the other.");
  }
  
  colnames(res) <- c('lo', 'hi');
  
  if (plot) {
    if ((length(d) > 1) || (length(n) > 1) || (length(conf.level) > 1)) {
      warning("I can only produce a plot if you supply only one value for ",
              "arguments d, n, and conf.level!");
    } else {
      df <- data.frame(d = seq(min(res) - .5, max(res) + .5, .001));
      df$density <- withCallingHandlers(dd(df$d, df = n-2, populationD = d),
                                        warning = wHandler);
      
      cilo <- min(res);
      cihi <- max(res);
      dValue <- d;
      
      plot <- ggplot(df, aes(x=d, y=density)) +
        theme_bw() +
        theme(axis.title.x.top = element_blank()) +
        scale_x_continuous(sec.axis = dup_axis(breaks=c(cilo,
                                                        dValue,
                                                        cihi),
                                               labels=round(c(cilo,
                                                              dValue,
                                                              cihi), 2))) +
        geom_vline(aes(xintercept=cilo), linetype='dashed') +
        geom_vline(aes(xintercept=dValue), linetype='dashed') +
        geom_vline(aes(xintercept=cihi), linetype='dashed') +
        geom_ribbon(data=df[df$d >= min(res) & df$d <= max(res), ],
                    aes(ymin = 0, ymax=density),
                    fill='#cadded') +
        geom_segment(x = min(res),
                     xend = min(res),
                     y = 0,
                     yend = dd(min(res), df = n-2,
                               populationD = d),
                     color = '#2a5581', size=1.5) +
        geom_segment(x = max(res),
                     xend = max(res),
                     y = 0,
                     yend = dd(max(res), df = n-2,
                               populationD = d),
                     color = '#2a5581', size=1.5) +
        geom_line(size=1.5);
      attr(res, "plot") <- plot;
      class(res) <- 'cohensdCI';
    }
  }
  
  d <- paste0('d=', d);
  n <- paste0('n=', n);
  
  rownames(res) <- paste0(d, ", ", n);

  if ((!silent) && (length(myWarnings) > 0)) {
    precisionWarnings <- grepl("full precision may not have been achieved in 'pnt{final}'",
                               myWarnings, fixed = TRUE);
    if (any(precisionWarnings)) {
      cat0("Function 'qt', which is used under the hood of this function (see ?qt for more information), ",
           "warned that 'full precision may not have been achieved'. ",
           "This is normally no cause for concern, because with sample sizes this big, small deviations ",
           "have little impact, but informing you seemed appropriate nonetheless.\n\n");
    }
    if (!all(precisionWarnings)) {
      cat0("One or more ", ifelse(any(precisionWarnings), "additional", ""),
           " warnings were encountered:\n");
      lapply(myWarnings[!precisionWarnings], function(x) cat0(x$message, "\n"));
      cat("\n");
    }
  }
  
  return(res);
}

print.cohensdCI <- function(x, ...) {
  ### Basically a trick because we're passing the plot as an attribute.
  if (!is.null(attr(x, 'plot'))) {
    grid.draw(attr(x, 'plot'));
    ### So remove the plot
    attr(x, 'plot') <- NULL;
  }
  ### And then remove the class to print
  print(unclass(x));
}

# ggplot(data.frame(x = seq(-3, 3, by=.1),
#                   d = dCohensd(seq(-3, 3, by=.1), populationD = .5, 18),
#                   d2 = dCohensd(seq(-3, 3, by=.1), populationD = .5, 180),
#                   t = dt(seq(-3, 3, by=.1), 18)),
#        aes(x=x)) +
#   geom_line(aes(y=d), color='red') +
#   geom_line(aes(y=d2), color='green') +
#   geom_line(aes(y=t), color='blue') +
#   theme_bw();
