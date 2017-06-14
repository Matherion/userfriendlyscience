# ggProportionPlot <- function(dat,
#                              items,
#                              conf.level=.95,
#                              method="binom.test",
#                              colors=brewer.pal(9, 'Set1')) {
#   
#   freqs <- 
#     sapply(dat[, items], table);
#   
#   totals <- colSums(freqs);
#   
#   proportions <- freqs/totals;
#   
#   print(freqs)
#   
#   print(totals)
#   
#   print(proportions);
#   
#   
# 
#   longDat <- data.frame(item = rep(items, each=nrow(dat)),
#                         value = unlist(dat[, items]));
#   plot <- ggplot(longDat, aes_string(x='item', y='value')) +
#     geom_point() + coord_flip();
#   print(plot);
# }
#                                
# ggProportionPlot(mtcars, items=c('vs', 'am'))

