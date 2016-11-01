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

qCohensd <- qd <- function(q, df, populationD = 0, lower.tail=TRUE) {
  ### Return Cohen's d for given p-value
  return(convert.t.to.d(qt(q, df,
                           ncp=convert.d.to.t(populationD, df + 2),
                           lower.tail=lower.tail), df + 2));
}

rCohensd <- rd <- function(n, df, populationD = 0) {
  ### Return random Cohen's d value(s)
  return(convert.t.to.d(rt(n, df,
                           ncp=convert.d.to.t(populationD, df + 2)),
                        df=df));
}

pdInterval <- function(ds, n) {
  return(pd(max(ds), n - 2) - pd(min(ds), n - 2));
}

pdExtreme <- function(d, n) {
  return(2 * pd(-1 * abs(d), n - 2));
}

pdMild <- function(d, n) {
  return(1 - pdExtreme(d, n));
}

# ggplot(data.frame(x = seq(-3, 3, by=.1),
#                   d = dCohensd(seq(-3, 3, by=.1), populationD = .5, 18),
#                   t = dt(seq(-3, 3, by=.1), 18)),
#        aes(x=x)) +
#   geom_line(aes(y=d), color='red') +
#   geom_line(aes(y=t), color='blue') +
#   theme_bw();
