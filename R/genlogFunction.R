####  Definition Generalized Logistic function (NB "B" is in exp()) with scaling factor
genlogFunction <- function(x, x0, Ab, At, B, v) {
  return(Ab + ((At - Ab)/ (1 + exp(-B*(x-x0)))**(1/v)));
}
