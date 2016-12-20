### Note: inspired by the answer of DeanK at http://stackoverflow.com/questions/15258233/using-table-caption-on-r-markdown-file-using-knitr-to-use-in-pandoc-to-convert-t 
### combined with http://gforge.se/2014/01/fast-track-publishing-using-knitr-part-iii/ by Max Gordon

setCaptionNumbering <- function (captionName = 'tab.cap',
                                 prefix = ":Table %s: ",
                                 suffix = "",
                                 romanNumeralSetting = "counter_roman") {
  hookFunction <- list(captionName = function(before, options, envir) {
    if (before) {
      cntr <- getOption("script_counter", 1);
      prefix <- sprintf(prefix, ifelse(getOption(romanNumeralSetting, FALSE), as.character(as.roman(cntr)), as.character(cntr)));
      suffix <- sprintf(suffix, ifelse(getOption(romanNumeralSetting, FALSE), as.character(as.roman(cntr)), as.character(cntr)));
      options(script_counter = cntr + 1);
      return(paste0(prefix, options[[captionName]], suffix));
    }
  });
  names(hookFunction) <- captionName;
  do.call(knitr::knit_hooks$set, hookFunction);
}
