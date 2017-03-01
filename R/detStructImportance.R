detStructImportance <- function(determinantStructure,
                                dat,
                                conf.level = list(means = .9999,
                                                  associations = .95),
                                subQuestions = NULL,
                                leftAnchors = rep("Lo", length(determinants)),
                                rightAnchors = rep("Hi", length(determinants)),
                                orderBy = NULL,
                                decreasing = NULL,
                                generateColors = list(means = c("red", "blue", "green"),
                                                      associations = c("red", "grey", "green")),
                                strokeColors = brewer.pal(9, 'Set1'),
                                titlePrefix = "Means and associations with",
                                titleSuffix = "",
                                fullColorRange = NULL,
                                associationsAlpha = .5,
                                theme=theme_bw(),
                                ...) {

  determinantStructure$Do(function(currentNode) {
    targets <- currentNode$Get('name', traversal='ancestor',
                               filterFun=function(x)
                                 return(x$type=='determinantVar'));
    currentSubQuestions <- ifelseObj(is.null(currentNode$subQuestions),
                                     subQuestions,
                                     currentNode$subQuestions);
    currentLeftAnchors <- ifelseObj(is.null(currentNode$leftAnchors),
                                    leftAnchors,
                                    currentNode$leftAnchors);
    currentRightAnchors <- ifelseObj(is.null(currentNode$rightAnchors),
                                     rightAnchors,
                                     currentNode$rightAnchors);
    determinants <- ifelseObj(currentNode$type=="subdeterminantProducts",
                              unlist(currentNode$productVarNames),
                              unlist(currentNode$varNames));
    currentNode$determinantImportance <-
      determinantImportance(data = dat,
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
                            returnPlotOnly = TRUE,
                            drawPlot = FALSE,
                            theme=theme,
                            ...);
  }, traversal = 'level', filterFun = function(x)
    return((x$type=="subdeterminants" || x$type=="subdeterminantProducts")));

}
