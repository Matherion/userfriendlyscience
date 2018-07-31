#' A function to conveniently process LimeSurvey labels
#' 
#' This function is meant to quickly parse the variable labels set by
#' LimeSurvey. It works particularly well with dual anchor array questions,
#' where the left and right anchors as well as the subquestions are extracted
#' automatically.
#' 
#' This function processes LimeSurvey variable labels and applies regular
#' expressions to automatically extract subquestions and left and right
#' anchors.
#' 
#' @param dat The dataframe as produced by \code{\link{importLimeSurveyData}}.
#' @param varnameRegExPairs Pairs of regular expressions to replace in the
#' variable names. This is useful when some pattern can be applied to the
#' variable names to, for example, add underscores te denote different parts of
#' the variable name.  This has to be a list of character vectors that each
#' have length 2.
#' @param labelExtractionRegExPair The regular expression pair used to extract
#' the labels.
#' @param lengthToWrap At how many characters to wrap the subquestions.
#' @param lengthToWrapAnchors At how many characters to wrap the anchors.
#' @param leftAnchorRegExPairs The regular expression pairs to use to extract
#' the left anchors.
#' @param rightAnchorRegExPairs The regular expression pairs to use to extract
#' the right anchors.
#' @return A dataframe that can conveniently be used with
#' \code{\link{detStructAddVarLabels}}.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{determinantStructure}}, \code{\link{determinantVar}},
#' \code{\link{subdeterminants}}, \code{\link{subdeterminantProducts}},
#' \code{\link{detStructAddVarLabels}}, \code{\link{detStructAddVarNames}},
#' \code{\link{detStructComputeProducts}},
#' \code{\link{detStructComputeScales}}, \code{\link{detStructCIBER}}
#' @references (Forthcoming)
#' @keywords utilities
#' @examples
#' 
#' ### No examples provided yet; this would require data to be included,
#' ### and that's not available yet.
#' 
#' @export processLSvarLabels
processLSvarLabels <- function(dat,
                               varnameRegExPairs = NULL,
                               labelExtractionRegExPair = c("\\[(.*)\\].*", "\\1"),
                               lengthToWrap = 50,
                               lengthToWrapAnchors = 20,
                               leftAnchorRegExPairs = list(c(".*[[:graph:]]([A-Z][a-z][^|]*)\\|(.+)", "\\1")),
                               rightAnchorRegExPairs = list(c(".*[[:graph:]]([A-Z][a-z][^|]*)\\|(.+)", "\\2"))) {

  labelDat <- data.frame(varNames.raw = names(dat),
                         varLabels.raw = attributes(dat)$variable.labels,
                         stringsAsFactors = FALSE);

  labelDat$varNames.cln <- labelDat$varNames.raw;

  if (!is.null(varnameRegExPairs)) {
    for (i in 1:length(varnameRegExPairs)) {
      labelDat$varNames.cln <- gsub(varnameRegExPairs[[i]][1],
                                    varnameRegExPairs[[i]][2],
                                    labelDat$varNames.cln);
    }
  }

  labelDat$varLabels.cln <- sub(labelExtractionRegExPair[1],
                                labelExtractionRegExPair[2],
                                labelDat$varLabels.raw);

  labelDat$leftAnchors <- "";
  labelDat$rightAnchors <- "";

  if (!is.null(leftAnchorRegExPairs)) {
    for (i in 1:length(leftAnchorRegExPairs)) {
      labelDat$leftAnchors <- ifelse(grepl(leftAnchorRegExPairs[[i]][1],
                                           labelDat$varLabels.cln),
                                     gsub(leftAnchorRegExPairs[[i]][1],
                                          leftAnchorRegExPairs[[i]][2],
                                          labelDat$varLabels.cln),
                                     labelDat$leftAnchors);
    }
  }

  if (!is.null(rightAnchorRegExPairs)) {
    for (i in 1:length(rightAnchorRegExPairs)) {
      labelDat$rightAnchors <- ifelse(grepl(rightAnchorRegExPairs[[i]][1],
                                            labelDat$varLabels.cln),
                                      gsub(rightAnchorRegExPairs[[i]][1],
                                           rightAnchorRegExPairs[[i]][2],
                                           labelDat$varLabels.cln),
                                      labelDat$rightAnchors);
    }
  }

  if (!is.null(leftAnchorRegExPairs)) {
    labelDat$subQuestions <- sapply(1:nrow(labelDat),
                                    function(rowNr) {
                                      return(sub(paste0("^(.*)", escapeRegex(labelDat$leftAnchors[rowNr]),
                                                        "\\|", escapeRegex(labelDat$rightAnchors[rowNr])),
                                                 "\\1",
                                                 labelDat$varLabels.cln[rowNr]));
                                    });

  }

  labelDat$questionText <- trim(sub("\\[.*\\](.*)", "\\1", labelDat$varLabels.raw));

  if (lengthToWrap > 0) {
    labelDat$questionText <- sapply(labelDat$questionText, function(x)
      return(paste0(strwrap(x, lengthToWrap), collapse="\n")));
    labelDat$subQuestions <- sapply(labelDat$subQuestions, function(x)
      return(paste0(strwrap(x, lengthToWrap), collapse="\n")));
  }
  if (lengthToWrapAnchors > 0) {
    labelDat$leftAnchors <- sapply(labelDat$leftAnchors, function(x)
      return(paste0(strwrap(x, lengthToWrapAnchors), collapse="\n")));
    labelDat$rightAnchors <- sapply(labelDat$rightAnchors, function(x)
      return(paste0(strwrap(x, lengthToWrapAnchors), collapse="\n")));
  }

  return(labelDat);

}
