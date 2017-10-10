extractVarName <- function(x) {
  regexpr <- "[[:alnum:]]+\\[[[:alnum:]]*,[[:blank:]]*['\"]([[:alnum:]]+)['\"]\\]";
  if (grepl(regexpr, x))
    return (sub(regexpr, "\\1", x))
  else
    return(sub(".*\\$(.*?)[])]*$", '\\1', x));
  ### Extract last expression following a dollar sign and possibly
  ### followed by parentheses or brackets
  #return(sub(".*\\$(.*?)[])]*$", '\\1', x));
}
