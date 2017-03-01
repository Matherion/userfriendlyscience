detStructAddVarLabels <- function(determinantStructure,
                                  varLabelDf,
                                  varNameCol = 'varNames.cln',
                                  leftAnchorCol = 'leftAnchors',
                                  rightAnchorCol = 'rightAnchors',
                                  subQuestionCol = 'subQuestions',
                                  questionTextCol = 'questionText') {
  
  determinantStructure$Do(function(currentNode) {
    currentNode$leftAnchors <- varLabelDf[varLabelDf[, varNameCol] %in%
                                            unlist(currentNode$varNames),
                                          leftAnchorCol];
    currentNode$rightAnchors <- varLabelDf[varLabelDf[, varNameCol] %in%
                                             unlist(currentNode$varNames),
                                           rightAnchorCol];
    currentNode$subQuestions <- varLabelDf[varLabelDf[, varNameCol] %in%
                                             unlist(currentNode$varNames),
                                           subQuestionCol];
    currentNode$questionTexts <- varLabelDf[varLabelDf[, varNameCol] %in%
                                              unlist(currentNode$varNames),
                                            questionTextCol];
  }, traversal = 'level', filterFun = function(x) return(!is.null(x$varNames)));
  
}
