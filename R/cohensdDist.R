dCohensd <- function(x, df, populationD = 0) {
  if (populationD != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  return(dt(convert.d.to.t(x, df + 2), df));
}

pCohensd <- function(q, df, populationD = 0, lower.tail=TRUE) {
  if (populationD != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  return(pt(convert.d.to.t(q, df + 2), df, lower.tail=lower.tail));
}

qCohensd <- function(q, df, populationD = 0, lower.tail=TRUE) {
  if (populationD != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  return(convert.t.to.d(qt(q, df, lower.tail=lower.tail), df + 2));
}

rCohensd <- function(n, df, populationD = 0) {
  if (populationD != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  return(convert.t.to.d(rt(n, df), df=df));
}
