### Credits:
### http://mathematicalcoffee.blogspot.nl/2014/06/ggpie-pie-graphs-in-ggplot2.html

ggPie <- function (vector, scale_fill = scale_fill_viridis(discrete=TRUE)) {
  dat <- data.frame(table(vector));
  names(dat) <- c('labels', 'totals');
  totals = 'totals';
  by = 'labels';
  
  dat <- dat[dat$totals > 0, ];
  
  return(ggplot(dat, aes_string(x=factor(1), y=totals, fill=by)) +
           geom_bar(stat='identity', color='black') +
           # removes black borders from legend
           guides(fill=guide_legend(override.aes=list(color=NA))) +
           coord_polar(theta='y') +
           scale_y_continuous(breaks=(sum(dat[[totals]]) - (cumsum(dat[[totals]]) - dat[[totals]] / 2)),
                              labels=dat[[by]]) +
           scale_fill +
           theme(axis.ticks=element_blank(),
                 axis.text.y=element_blank(),
                 axis.text.x=element_text(color='black'),
                 axis.title=element_blank(),
                 legend.position="none",
                 panel.background = element_rect(fill = "white")));
}
