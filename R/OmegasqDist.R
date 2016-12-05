# domegaSq <- function(x, df1, df2, populationOmegaSq = 0) {
#   if (populationOmegaSq != 0) {
#     cat0("Noncentrality parameters not implemented yet, sorry!\n");
#   }
#   ### Return density for given omega squared
#   return(df(convert.omegasq.to.f(x, df1, df2), df1, df2));
# }

pomegaSq <- function(q, df1, df2, populationOmegaSq = 0, lower.tail=TRUE) {
  if (populationOmegaSq != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  ### Return p-value for given omega squared
  return( pf(convert.omegasq.to.f(q, df1, df2), df1, df2, lower.tail=lower.tail) );
}

qomegaSq <- function(p, df1, df2, populationOmegaSq = 0, lower.tail=TRUE) {
  if (populationOmegaSq != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  ### Return omega squared for given p-value
  return(convert.f.to.omegasq(qf(p, df1, df2, lower.tail=lower.tail), df1, df2));
}

romegaSq <- function(n, df1, df2, populationOmegaSq = 0) {
  if (populationOmegaSq != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  ### Return random omega squared value(s)
  return(convert.f.to.omegasq(rf(n, df1, df2), df1, df2));
}

domegaSq <- function(x, df1, df2, populationOmegaSq = 0) {
  return(df(convert.omegasq.to.f(x, df1, df2), df1, df2,
            ncp = convert.omegasq.to.f(populationOmegaSq, df1, df2)));
}

# pomegaSq <- function(q, df1, df2, populationOmegaSq = 0, lower.tail=TRUE) {
#   if (populationOmegaSq != 0) {
#     cat0("Noncentrality parameters not implemented yet, sorry!\n");
#   }
#   ### Return p-value for given omega squared
#   return( pf(convert.omegasq.to.f(q, df1, df2), df1, df2,
#              lower.tail=lower.tail,
#              ncp = convert.omegasq.to.f(populationOmegaSq, df1, df2)) );
# }
# 
# qomegaSq <- function(p, df1, df2, populationOmegaSq = 0, lower.tail=TRUE) {
#   if (populationOmegaSq != 0) {
#     cat0("Noncentrality parameters not implemented yet, sorry!\n");
#   }
#   ### Return omega squared for given p-value
#   return(convert.f.to.omegasq(qf(p, df1, df2,
#                                  lower.tail=lower.tail,
#                                  ncp = convert.omegasq.to.f(populationOmegaSq, df1, df2)),
#                               df1, df2));
# }
# 
# romegaSq <- function(n, df1, df2, populationOmegaSq = 0) {
#   if (populationOmegaSq != 0) {
#     cat0("Noncentrality parameters not implemented yet, sorry!\n");
#   }
#   ### Return random omega squared value(s)
#   return(convert.f.to.omegasq(rf(n, df1, df2,
#                                  ncp = convert.omegasq.to.f(populationOmegaSq, df1, df2)),
#                               df1, df2));
# }

