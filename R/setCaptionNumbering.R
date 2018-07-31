### Necessary to be able to set knitr hooks. Apparently.
#utils::globalVariables(c("before"))



#' Convenience function for numbered captions in knitr (and so, RMarkdown)
#' 
#' This function makes it easy to tell knitr (and so RMarkdown) to use numbered
#' captions of any type.
#' 
#' 
#' @param captionName The name of the caption; this is used both as unique
#' identifier for the counter, and to set the caption text (included between
#' the prefix and suffix) in the chunk options.
#' @param prefix The text to add as prefix before the action caption; this will
#' typically include '\%s\%' which will be replaced by the number of this
#' caption.
#' @param suffix The text to add as suffix after the action caption; this can
#' also include '\%s\%' which will be replaced by the number of this caption.
#' Together with the \code{prefix}, this can also be used to enclose the
#' caption in html.
#' @param captionBefore Whether the caption should appear before or after the
#' relevant chunk output.
#' @param romanNumeralSetting The name of the option (should be retrievable
#' with \code{\link{getOption}}) where it's configured whether to use Roman
#' (TRUE) or Latin (FALSE) numerals. FALSE is assumed if this option isn't set.
#' @param optionName The name of the option to use to retrieve and set the
#' counter. This can be used, for example, to have multiple caption types use
#' the same counter.
#' @param resetCounterTo If not \code{NULL} and numeric, the counter will start
#' at this number.
#' @return This function returns nothing, but instead sets the appropriate
#' \code{\link{knit_hooks}}. Or rather, just one hook.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utils
#' @examples
#' 
#' \dontrun{
#'   setCaptionNumbering(captionName='tab.cap',
#'                       prefix = ":Table %s: ");
#' }
#' 
#' @export setCaptionNumbering
setCaptionNumbering <- function(captionName = 'tab.cap',
                                prefix = ":Table %s: ",
                                suffix = "",
                                captionBefore = FALSE,
                                romanNumeralSetting = "counter_roman",
                                optionName = paste0('setCaptionNumbering_', captionName),
                                resetCounterTo = 1) {
  if (!is.null(resetCounterTo) && is.numeric(resetCounterTo)) {
    do.call('options', as.list(structure(resetCounterTo,
                                         names=optionName)));
  }
  if (captionBefore) {
    hookFunction <- list(captionName = function(before, options, envir) {
      if (before) {
        cntr <- getOption(optionName, 1);
        if (!is.numeric(cntr)) cntr <- 1;
        prefix <- sprintf(prefix, ifelse(getOption(romanNumeralSetting, FALSE), as.character(as.roman(cntr)), as.character(cntr)));
        suffix <- sprintf(suffix, ifelse(getOption(romanNumeralSetting, FALSE), as.character(as.roman(cntr)), as.character(cntr)));
        do.call('options', as.list(structure(cntr+1, names=optionName)));
        return(paste0(prefix, options[[captionName]], suffix));
      }
    });
  } else {
    hookFunction <- list(captionName = function(before, options, envir) {
      if (!before) {
        cntr <- getOption(optionName, 1);
        if (!is.numeric(cntr)) cntr <- 1;
        prefix <- sprintf(prefix, ifelse(getOption(romanNumeralSetting, FALSE), as.character(as.roman(cntr)), as.character(cntr)));
        suffix <- sprintf(suffix, ifelse(getOption(romanNumeralSetting, FALSE), as.character(as.roman(cntr)), as.character(cntr)));
        do.call('options', as.list(structure(cntr+1, names=optionName)));
        return(paste0(prefix, options[[captionName]], suffix));
      }
    });
  }
  names(hookFunction) <- captionName;
  do.call(knitr::knit_hooks$set, hookFunction);
}
