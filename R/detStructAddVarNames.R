detStructAddVarNames <- function(determinantStructure,
                                 names) {
  
  ### Get all behaviorRegExes that are set (should only be one)
  behaviorRegEx <- determinantStructure$Get('behaviorRegEx',
                                            traversal='level',
                                            filterFun=function(x) return(!is.null(x$behaviorRegEx)));
  
  ### Remove any duplicates and select the first one in case there are more
  behaviorRegEx <- unique(behaviorRegEx)[1];
  
  ### Only retain the names matching that behavior regex
  names <- grep(behaviorRegEx, names, value=TRUE);
  
  ### Walk through the determinant structure and select the
  ### matching variable names, adding the to the structure
  determinantStructure$Do(function(currentNode, allNames = names) {
    if (is.list(currentNode$selection)) {
      currentNode$varNames <- sapply(currentNode$selection,
                                     function(x) {
                                       res <- sapply(x,
                                                     grep,
                                                     allNames,
                                                     value=TRUE,
                                                     simplify=FALSE);
                                       names(res) <- allNames;
                                       return(res);
                                     },
                                     simplify=FALSE);
      names(currentNode$varNames) <- currentNode$selection;
    } else {
      currentNode$varNames <- sapply(currentNode$selection,
                                     grep, allNames, value=TRUE, simplify=FALSE);
      names(currentNode$varNames) <- currentNode$selection;
    }
  }, traversal = 'level', filterFun = function(x) return(!is.null(x$selection)));
  
}
