extractVarName <- function(x) {
  ### Extract last expression following a dollar sign and possibly
  ### followed by parentheses or brackets
  return(sub(".*\\$(.*?)[])]*$", '\\1', x));
}
