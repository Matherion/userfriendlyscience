dCohensd <- dd <- function(x, df, populationD = 0) {
  ### Return density for given Cohen's d
  return(dt(convert.d.to.t(x, df + 2), df,
            ncp=convert.d.to.t(populationD, df + 2)));
}

pCohensd <- pd <- function(q, df, populationD = 0, lower.tail=TRUE) {
  ### Return p-value for given Cohen's d
  return(pt(convert.d.to.t(q, df + 2), df,
            ncp=convert.d.to.t(populationD, df + 2),
            lower.tail=lower.tail));
}

qCohensd <- qd <- function(p, df, populationD = 0, lower.tail=TRUE) {
  ### Return Cohen's d for given p-value
  return(convert.t.to.d(qt(p, df,
                           ncp=convert.d.to.t(populationD, df + 2),
                           lower.tail=lower.tail), df + 2));
}

rCohensd <- rd <- function(n, df, populationD = 0) {
  ### Return random Cohen's d value(s)
  return(convert.t.to.d(rt(n, df,
                           ncp=convert.d.to.t(populationD, df + 2)),
                        df=df));
}

pdInterval <- function(ds, n, populationD = 0) {
  return(pd(max(ds), n - 2, populationD=populationD) -
           pd(min(ds), n - 2, populationD=populationD));
}

pdExtreme <- function(d, n, populationD = 0) {
  return(2 * pd(d, n - 2, populationD=populationD,
                lower.tail = (d <= populationD)));
}

pdMild <- function(d, n, populationD = 0) {
  return(1 - pdExtreme(d, n, populationD=populationD));
}

cohensdCI <- function(d, n, conf.level = .95, plot=FALSE) {
  ci.bound.lo <- (1 - conf.level) / 2;
  ci.bound.hi <- 1 - (1 - conf.level) / 2;
  if (length(d) == length(n)) {
    res <- t(sapply(1:length(d), function(i) {
      return(c(qCohensd(ci.bound.lo, n[i], populationD=d[i]),
               qCohensd(ci.bound.hi, n[i], populationD=d[i])));
    }));
  } else if ((length(d) == 1) || (length(n) == 1)) {
    res <- matrix(c(qCohensd(ci.bound.lo, n, populationD=d),
                    qCohensd(ci.bound.hi, n, populationD=d)), ncol=2);
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
      df$density <- dd(df$d, df = n-2, populationD = d);

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
