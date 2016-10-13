dd <- function(x, df) {
  return(dt(convert.d.to.t(x, df), df));
}

pd <- function(q, df) {
  return(pt(convert.d.to.t(q, df), df));
}

qd <- function(p, df) {
  return(convert.t.to.d(qt(p, df), df));
}

rd <- function(n, df) {
  return(convert.t.to.d(rt(n, df), df));
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
#                   d = dd(seq(-3, 3, by=.1), 18),
#                   t = dt(seq(-3, 3, by=.1), 18)),
#        aes(x=x)) +
#   geom_line(aes(y=d), color='red') +
#   geom_line(aes(y=t), color='blue') +
#   theme_bw();
