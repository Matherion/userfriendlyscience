### Arguments that are evaluated only after
### they got a value
globalVariables(c('determinants'));

detStructCIBER <- function(determinantStructure,
                           dat,
                           conf.level = list(means = .9999,
                                             associations = .95),
                           subQuestions = NULL,
                           leftAnchors = rep("Lo", length(determinants)),
                           rightAnchors = rep("Hi", length(determinants)),
                           orderBy = 1,
                           decreasing = NULL,
                           generateColors = list(means = c("red", "blue", "green"),
                                                 associations = c("red", "grey", "green")),
                           strokeColors = brewer.pal(9, 'Set1'),
                           titlePrefix = "Means and associations with",
                           titleVarLabels = NULL,
                           titleSuffix = "",
                           fullColorRange = NULL,
                           associationsAlpha = .5,
                           baseSize = .8,
                           dotSize = baseSize,
                           baseFontSize=10*baseSize,
                           theme=theme_bw(base_size=baseFontSize),
                           ...) {

  determinantStructure$Do(function(currentNode) {
    varNames <- currentNode$Get('name', traversal='ancestor',
                                filterFun=function(x)
                                  return(x$type=='determinantVar'));
    scaleVarNames <- currentNode$Get('scaleVarName', traversal='ancestor',
                                     filterFun=function(x)
                                     return(x$type=='determinantVar'));
    targets <- ifelseObj(is.null(scaleVarNames), varNames, scaleVarNames);
    currentSubQuestions <- ifelseObj(is.null(currentNode$subQuestions),
                                     subQuestions,
                                     currentNode$subQuestions);
    determinants <- ifelseObj(currentNode$type=="subdeterminantProducts",
                              unlist(currentNode$productVarNames),
                              unlist(currentNode$varNames));
    currentLeftAnchors <- ifelseObj(is.null(currentNode$leftAnchors),
                                    leftAnchors,
                                    currentNode$leftAnchors);
    currentRightAnchors <- ifelseObj(is.null(currentNode$rightAnchors),
                                     rightAnchors,
                                     currentNode$rightAnchors);

    if (is.numeric(orderBy)) {
      orderBy <- targets[orderBy];
    }

    currentNode$determinantImportance <-
      CIBER(data = dat,
            determinants = determinants,
            targets = targets,
            conf.level = conf.level,
            subQuestions = currentSubQuestions,
            leftAnchors = currentLeftAnchors,
            rightAnchors = currentRightAnchors,
            orderBy = orderBy,
            decreasing = decreasing,
            generateColors = generateColors,
            strokeColors = strokeColors,
            titlePrefix = titlePrefix,
            titleSuffix = titleSuffix,
            fullColorRange = fullColorRange,
            associationsAlpha = associationsAlpha,
            titleVarLabels=varNames,
            returnPlotOnly = TRUE,
            drawPlot = FALSE,
            baseFontSize=baseFontSize,
            theme=theme,
            ...);
  }, traversal = 'level', filterFun = function(x)
    return((x$type=="subdeterminants" || x$type=="subdeterminantProducts")));

}
