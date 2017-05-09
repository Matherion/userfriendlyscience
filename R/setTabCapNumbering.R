setTabCapNumbering <- function(table_counter_str = ":Table %s: ",
                               resetCounterTo = 1) {
  setCaptionNumbering(captionName = 'tab.cap',
                      prefix = table_counter_str,
                      suffix = "",
                      resetCounterTo = resetCounterTo);  
}
