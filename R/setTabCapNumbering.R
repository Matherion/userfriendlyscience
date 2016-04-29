### Note: inspired by the answer of DeanK at http://stackoverflow.com/questions/15258233/using-table-caption-on-r-markdown-file-using-knitr-to-use-in-pandoc-to-convert-t 
### combined with http://gforge.se/2014/01/fast-track-publishing-using-knitr-part-iii/ by Max Gordon


setTabCapNumbering <- function(table_counter_str = ":Table %s: ") {

  knit_hooks$set(tab.cap = function(before, options, envir) {
    if (!before) {
      cntr <- getOption("table_counter", FALSE);
      if (cntr != FALSE) {
        if (is.logical(cntr)) cntr <- 1
        tab_number_txt <-
          sprintf(getOption("table_counter_str", table_counter_str), 
                  ifelse(getOption("table_counter_roman", FALSE), 
                         as.character(as.roman(cntr)), as.character(cntr)));
        options(table_counter = cntr + 1);
      }
      return(paste0(tab_number_txt, options$tab.cap));
    }
  });

}
