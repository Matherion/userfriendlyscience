### Necessary to be able to set knitr hooks. Apparently.
utils::globalVariables(c("before"))

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
    hookFunction <- list(captionName = function(after, options, envir) {
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
