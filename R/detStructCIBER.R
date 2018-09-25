### Arguments that are evaluated only after they got a value
utils::globalVariables(c('determinants', 'targets'));

#'@rdname CIBER
#'@export
detStructCIBER <- function(determinantStructure,
                           data,
                           conf.level = list(means = .9999,
                                             associations = .95),
                           subQuestions = NULL,
                           leftAnchors = rep("Lo", length(determinants)),
                           rightAnchors = rep("Hi", length(determinants)),
                           orderBy = 1,
                           decreasing = NULL,
                           generateColors = list(means = c("red", "blue", "green"),
                                                 associations = c("red", "grey", "green")),
                           strokeColors = NULL,
                           titlePrefix = "Means and associations with",
                           titleVarLabels = NULL,
                           titleSuffix = "",
                           fullColorRange = NULL,
                           associationsAlpha = .5,
                           baseSize = .8,
                           dotSize = 2.5 * baseSize,
                           baseFontSize=10*baseSize,
                           theme=ggplot2::theme_bw(base_size=baseFontSize),
                           ...) {
  
  determinantStructure$Do(function(currentNode) {
    
    varNames <- currentNode$Get('name', traversal='ancestor',
                                filterFun=function(x)
                                  return(x$type=='determinantVar'));
    
    scaleVarNames <- currentNode$Get('scaleVarName', traversal='ancestor',
                                     filterFun=function(x)
                                       return(x$type=='determinantVar'));
    
    targets <-
      ufs::ifelseObj(is.null(scaleVarNames),
                     varNames,
                     scaleVarNames);
    
    currentSubQuestions <-
      ufs::ifelseObj(is.null(currentNode$subQuestions),
                     subQuestions,
                     currentNode$subQuestions);
    determinants <-
      ufs::ifelseObj(currentNode$type=="subdeterminantProducts",
                     unlist(currentNode$productVarNames),
                     unlist(currentNode$varNames));
    currentLeftAnchors <-
      ufs::ifelseObj(is.null(currentNode$leftAnchors),
                     leftAnchors,
                     currentNode$leftAnchors);
    currentRightAnchors <-
      ufs::ifelseObj(is.null(currentNode$rightAnchors),
                     rightAnchors,
                     currentNode$rightAnchors);
    
    if (is.numeric(orderBy)) {
      orderBy <- targets[orderBy];
    }
    
    currentNode$determinantImportance <-
      #behaviorchange::CIBER(data = data,
      CIBER(data = data,
                            determinants = determinants,
                            targets = targets,
                            conf.level = conf.level,
                            subQuestions = currentSubQuestions,
                            leftAnchors = currentLeftAnchors,
                            rightAnchors = currentRightAnchors,
                            orderBy = orderBy,
                            decreasing = decreasing,
                            generateColors = generateColors,
                            strokeColors = NULL,
                            titlePrefix = titlePrefix,
                            titleSuffix = titleSuffix,
                            fullColorRange = fullColorRange,
                            associationsAlpha = associationsAlpha,
                            baseSize = baseSize,
                            dotSize = dotSize,
                            titleVarLabels=varNames,
                            returnPlotOnly = TRUE,
                            drawPlot = FALSE,
                            baseFontSize=baseFontSize,
                            theme=theme,
                            ...);
  }, traversal = 'level', filterFun = function(x)
    return((x$type=="subdeterminants") ||
             (x$type=="subdeterminantProducts"))
  );
  
}

#detStructCIBER <- behaviorchange::detStructCIBER;
