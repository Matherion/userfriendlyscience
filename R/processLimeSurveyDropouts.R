processLimeSurveyDropouts <- function(lastpage, pagenames = NULL,
                                      relevantPagenames = NULL) {

  res <- list();
  res$specificDropout <- data.frame(lastpage = 0:max(lastpage));

  if (is.null(pagenames)) pagenames <-
      paste('Dropped out at page', seq(from=1, to=max(lastpage + 1)));

  if (is.null(relevantPagenames)) relevantPagenames <-
      paste('Page', seq(from=1, to=max(lastpage + 1)));

  if (length(pagenames) != nrow(res$specificDropout)) {
    stop("The vector 'pagenames' must have the same length as the number of pages ",
         "in the 'lastpage' vector - but ", length(pagenames), " pagenames were ",
         "provided, for ", nrow(res$specificDropout), " lastpages.");
  }

  totalParticipants <- length(lastpage);

  res$specificDropout <- merge(res$specificDropout,
                               as.data.frame(table(lastpage),
                                             responseName='frequency'),
                               by='lastpage',
                               all=TRUE);
  res$specificDropout$frequency[is.na(res$specificDropout$frequency)] <- 0;
  res$specificDropout <- res$specificDropout[order(as.numeric(res$specificDropout$lastpage)), ];

  res$specificDropout$comments <- pagenames;

  res$progressiveDropout <- data.frame(frequency = totalParticipants -
                                         head(c(0, tail(cumsum(res$specificDropout$frequency), -1)), -1));
  res$progressiveDropout$percentage <- 100 * res$progressiveDropout$frequency /
    totalParticipants;
  res$progressiveDropout$page <- 1:nrow(res$progressiveDropout);

  res$plots -> list;

  res$plots$absoluteDropout <-
    ggplot(res$progressiveDropout, aes_string(x='page', y='frequency')) +
    geom_point(size=4) + geom_line(size=1) + ylab('Number of participants') +
    xlab('Page in the questionnaire') + theme_bw() +
    geom_text_repel(aes_string(label='frequency'),
                    point.padding = unit(1, 'lines'),
                    min.segment.length = unit(0.05, "lines"),
                    segment.color="#2A5581", color="#2A5581",
                    size=5, nudge_x=1) +
    scale_x_continuous(breaks=res$progressiveDropout$page);
  res$plots$relativeDropout <-
    ggplot(res$progressiveDropout, aes_string(x='page', y='percentage')) +
    geom_point(size=4) + geom_line(size=1) + ylab('Percentage of participants') +
    xlab('Page in the questionnaire') + theme_bw() +
    geom_text_repel(aes(label=paste0(round(res$progressiveDropout$percentage), "%")),
                    point.padding = unit(1, 'lines'),
                    min.segment.length = unit(0.05, "lines"),
                    segment.color="#2A5581", color="#2A5581",
                    size=5, nudge_x=1) +
    scale_x_continuous(breaks=res$progressiveDropout$page);

  class(res) <- 'limeSurveyDropouts';

  return(res);

}
