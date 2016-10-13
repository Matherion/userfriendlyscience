dCohensd <- function(x, df, populationD = 0) {
  ### Return density for given Cohen's d
  return(dt(convert.d.to.t(x, df + 2), df,
            ncp=convert.d.to.t(populationD, df + 2)));
}

pCohensd <- function(q, df, populationD = 0, lower.tail=TRUE) {
  ### Return p-value for given Cohen's d
  return(pt(convert.d.to.t(q, df + 2), df,
            ncp=convert.d.to.t(populationD, df + 2),
            lower.tail=lower.tail));
}

qCohensd <- function(q, df, populationD = 0, lower.tail=TRUE) {
  ### Return Cohen's d for given p-value
  return(convert.t.to.d(qt(q, df,
                           ncp=convert.d.to.t(populationD, df + 2),
                           lower.tail=lower.tail), df + 2));
}

rCohensd <- function(n, df, populationD = 0) {
  if (populationD != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  ### Return random Cohen's d value(s)
  return(convert.t.to.d(rt(n, df),
                        ncp=convert.d.to.t(populationD, df + 2),
                        df=df));
}
