### Credits:
### http://mathematicalcoffee.blogspot.nl/2014/06/ggpie-pie-graphs-in-ggplot2.html

ggPie <- function (vector) {
  dat <- data.frame(table(vector));
  names(dat) <- c('labels', 'totals');
  totals = 'totals';
  by = 'labels';  
  return(ggplot(dat, aes_string(x=factor(1), y=totals, fill=by)) +
           geom_bar(stat='identity', color='black') +
           # removes black borders from legend
           guides(fill=guide_legend(override.aes=list(color=NA))) +
           coord_polar(theta='y') +
           scale_y_continuous(breaks=cumsum(dat[[totals]]) - dat[[totals]] / 2,
                              labels=dat[[by]]) +
           theme(axis.ticks=element_blank(),
                 axis.text.y=element_blank(),
                 axis.text.x=element_text(color='black'),
                 axis.title=element_blank(),
                 legend.position="none",
                 panel.background = element_rect(fill = "white")));
}
