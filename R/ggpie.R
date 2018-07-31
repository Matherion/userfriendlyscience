### Credits:
### http://mathematicalcoffee.blogspot.nl/2014/06/ggpie-pie-graphs-in-ggplot2.html



#' A ggplot pie chart
#' 
#' THis function creates a pie chart. Note that these are generally quite
#' strongly advised against, as people are not good at interpreting relative
#' frequencies on the basis of pie charts.
#' 
#' 
#' @param vector The vector (best to pass a factor).
#' @param scale_fill The ggplot scale fill function to use for the colors.
#' @return A ggplot pie chart.
#' @note This function is very strongly based on the Mathematical Coffee post
#' at
#' http://mathematicalcoffee.blogspot.com/2014/06/ggpie-pie-graphs-in-ggplot2.html.
#' @author Amy Chan; implemented in this package (and tweaked a bit) by
#' Gjalt-Jorn Peters.
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords hplot
#' @examples
#' 
#' ggPie(mtcars$cyl);
#' 
#' @export ggPie
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
