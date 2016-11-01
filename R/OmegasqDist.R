domegasq <- function(x, df1, df2, populationOmega = 0) {
  if (populationOmega != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  ### Return density for given omega squared
  return(df(convert.omegasq.to.f(x, df1, df2), df1, df2));
}

pomegasq <- function(q, df1, df2, populationOmega = 0, lower.tail=TRUE) {
  if (populationOmega != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  ### Return p-value for given omega squared
  return( pf(convert.omegasq.to.f(q, df1, df2), df1, df2, lower.tail=lower.tail) );
}

qomegasq <- function(p, df1, df2, populationOmega = 0, lower.tail=TRUE) {
  if (populationOmega != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  ### Return omega squared for given p-value
  return(convert.f.to.omegasq(qf(p, df1, df2, lower.tail=lower.tail), df1, df2));
}

romegasq <- function(n, df1, df2, populationOmega = 0) {
  if (populationOmega != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  ### Return random omega squared value(s)
  return(convert.f.to.omegasq(rf(n, df1, df2), df1, df2));
}
