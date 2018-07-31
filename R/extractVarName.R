#' Extract variable names
#' 
#' Functions often get passed variables from within dataframes or other lists.
#' However, printing these names with all their dollar signs isn't very
#' userfriendly. This function simply uses a regular expression to extract the
#' actual name.
#' 
#' 
#' @param x A character vector of one or more variable names.
#' @return The actual variables name, with all containing objectes stripped
#' off.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utils
#' @examples
#' 
#' extractVarName('mtcars$mpg');
#' 
#' @export extractVarName
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
