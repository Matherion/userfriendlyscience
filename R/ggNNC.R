ggNNC <- function(cerDataSeq, d = NULL,
                  eventDesirable = TRUE,
                  eventIfHigher = TRUE,
                  r = 1,
                  xlab = "Continuous outcome",
                  theme=theme_bw(),
                  lineSize=1,
                  cerColor = '#EBF2F8',
                  eerColor = "#172F47", #'#CADDED',
                  cerAlpha = .66,
                  eerAlpha = .66,
                  xlim = NULL,
                  showLegend = TRUE,
                  lineColor = "#000000",
                  verticalLineColor = "#172F47",
                  eventColor = "#00FF00",
                  eventAlpha = .2,
                  noEventColor = "#FF0000",
                  noEventAlpha = .2,
                  eventTextColor = "#009900",
                  noEventTextColor = "#990000",
                  dArrowDistance = .04 * max(cerDataSeq$density),
                  dLabelDistance = .08 * max(cerDataSeq$density)) {
  
  if (!('erDataSeq' %in% class(cerDataSeq))) {
    stop("As 'erDataSeq', you must pass an object of class 'erDataSeq', such as ",
         "the result of a call to function 'erDataSeq' (see ?erDataSeq for help).");
  }
  
  cer <- attr(cerDataSeq, 'er');
  
  if (!is.null(d)) d <- convert.r.to.d(convert.d.to.r(d) * r);
  if (!is.null(d)) {
    eer <- convert.d.to.eer(d, cer, eventDesirable=eventDesirable);
  } else {
    eer <- cer;
  }
  
  if (is.null(d)) d <- 0;
  
  sd <- attr(cerDataSeq, 'sd');
  cerValue <- attr(cerDataSeq, 'erValue');
  meanValue <- attr(cerDataSeq, 'meanValue');
  eerDataSeq <- cerDataSeq;
  eerDataSeq$x <- eerDataSeq$x + d * sd;
  newMeanValue <- meanValue + d * sd;
  cerValueDensity <- cerDataSeq[cerDataSeq$x == max(cerDataSeq[cerDataSeq$x < cerValue, 'x']), 'density'];
  eerValueDensity <- eerDataSeq[eerDataSeq$x == max(eerDataSeq[eerDataSeq$x < cerValue, 'x']), 'density'];
  cerLabel <- paste0("CER = ", round(cer, 2), ifelse(d > 0, "      ", ""));
  eerLabel <- paste0("EER = ", round(eer, 2));
  
  ### Compute sensible limits
  if (meanValue < newMeanValue) {
    lowestXWithDensity <- floor(max(cerDataSeq[cerDataSeq$density < .001 & cerDataSeq$x < meanValue, 'x']));
    highestXWithDensity <- ceiling(min(eerDataSeq[eerDataSeq$density < .001 & eerDataSeq$x > newMeanValue, 'x']));
  } else {
    lowestXWithDensity <- floor(max(eerDataSeq[eerDataSeq$density < .001 & eerDataSeq$x < newMeanValue, 'x']));
    highestXWithDensity <- ceiling(min(cerDataSeq[cerDataSeq$density < .001 & cerDataSeq$x > meanValue, 'x']));
  }
  if (is.null(xlim)) xlim <- c(lowestXWithDensity,
                               highestXWithDensity);
  
  ### Basic plot
  basePlot <- ggplot() + theme;
  
  ### If limits are specified, set them
  if (!is.null(xlim)) {
    basePlot <- basePlot + xlim(xlim);
  }
  
  ### Layer with CER normal curve
  if (eventIfHigher) {
    cerFill <- geom_ribbon(data = cerDataSeq[cerDataSeq$x > cerValue, ],
                           aes(x=x, ymax=density, ymin=0, fill=cerLabel), alpha=cerAlpha);
  } else  {
    cerFill <- geom_ribbon(data = cerDataSeq[cerDataSeq$x < cerValue, ],
                           aes(x=x, ymax=density, ymin=0, fill=cerLabel), alpha=cerAlpha);
  }
  ### Add line on top
  cerOutline <- geom_line(data=cerDataSeq, aes(x=x, y=density), size=lineSize, color=lineColor,
                          na.rm=TRUE);
  ### Vertical line to show CER
  cerLine <- geom_segment(aes(x=cerValue, xend=cerValue, y=0, yend=cerValueDensity),
                          size=lineSize, color=verticalLineColor);
  
  ### Layer with EER normal curve
  if (eventIfHigher) {
    eerFill <- geom_ribbon(data = eerDataSeq[eerDataSeq$x > cerValue, ],
                           aes(x=x, ymax=density, ymin=0, fill=eerLabel), alpha=eerAlpha);
  } else {
    eerFill <- geom_ribbon(data = eerDataSeq[eerDataSeq$x < cerValue, ],
                           aes(x=x, ymax=density, ymin=0, fill=eerLabel), alpha=eerAlpha);
  }
  ### Add line on top
  eerOutline <- geom_line(data=eerDataSeq, aes(x=x, y=density), size=lineSize, color=lineColor,
                          na.rm=TRUE);
  ### Vertical line to show EER
  eerLine <- geom_segment(aes(x=cerValue, xend=cerValue, y=0, yend=eerValueDensity),
                          size=lineSize, color=verticalLineColor);
  
  ### Indicator for difference between distributions
  dArrow <- geom_segment(aes(x = meanValue, xend = newMeanValue,
                             y = max(cerDataSeq$density) + dArrowDistance,
                             yend = max(cerDataSeq$density) + dArrowDistance),
                         arrow=arrow(length = unit(.02, 'npc'), ends='both', type='closed', angle=20),
                         size=lineSize, color=lineColor);
  dText <- geom_text(aes(x = mean(c(meanValue, newMeanValue)),
                         y = max(cerDataSeq$density) + dLabelDistance),
                     hjust=.5, label=paste0("d = ", round(d, 2)));
  
  ### Layer with box to display at the bottom
  if (eventIfHigher) {
    eventBarNoEvent <- geom_rect(aes(xmin = -Inf, xmax = cerValue, ymax = 0, ymin = -Inf),
                                 fill=noEventColor, alpha=noEventAlpha);
    eventBarEvent <- geom_rect(aes(xmin = cerValue, xmax = Inf, ymax = 0, ymin = -Inf),
                               fill=eventColor, alpha=eventAlpha);
    eventBarNoEventText <- geom_text(aes(x = mean(c(lowestXWithDensity, cerValue)),
                                         y = -.5*dArrowDistance,
                                         label=paste0('No event (< ', round(cerValue, 2), ")")),
                                     vjust=1, color = noEventTextColor);
    eventBarEventText <- geom_text(aes(x = mean(c(highestXWithDensity, cerValue)),
                                       y = -.5*dArrowDistance,
                                       label=paste0('Event (> ', round(cerValue, 2), ")")),
                                   vjust=1, color = eventTextColor);
  } else {
    eventBarNoEvent <- geom_rect(aes(xmin = cerValue, xmax = Inf, ymax = 0, ymin = -Inf),
                                 fill=noEventColor, alpha=noEventAlpha);
    eventBarEvent <- geom_rect(aes(xmin = -Inf, xmax = cerValue, ymax = 0, ymin = -Inf),
                               fill=eventColor, alpha=eventAlpha);
    eventBarNoEventText <- geom_text(aes(x = mean(c(highestXWithDensity, cerValue)),
                                         y = -.5*dArrowDistance,
                                         label=paste0('No event (< ', round(cerValue, 2), ")")),
                                     vjust=1, color = noEventTextColor);
    eventBarEventText <- geom_text(aes(x = mean(c(lowestXWithDensity, cerValue)),
                                       y = -.5*dArrowDistance,
                                       label=paste0('Event (> ', round(cerValue, 2), ")")),
                                   vjust=1, color = eventTextColor);
  }
  
  ### Horizontal line at 0 (just aesthetic)
  zeroLine <- geom_hline(aes(yintercept=0), color=lineColor, size=lineSize);
  
  ### Build & return plot
  basePlot <- basePlot +
    eventBarNoEvent + eventBarEvent +
    eventBarNoEventText + eventBarEventText;
  if (d>0) {
    # if (cerValueDensity > eerValueDensity) {
    # basePlot <- basePlot + cerFill + cerOutline + cerLine +
    #   eerFill + eerOutline + eerLine;
    # } else {
    basePlot <- basePlot + eerFill + eerOutline + eerLine +
      cerFill + cerOutline + cerLine;
    # }
    basePlot <- basePlot + dArrow + dText + zeroLine +
      scale_fill_manual(values = c(cerColor, eerColor), name="");
  } else {
    basePlot <- basePlot + eventBarNoEvent + eventBarEvent +
      cerFill + cerOutline + cerLine + zeroLine +
      scale_fill_manual(values = cerColor, name="");
    
  }
  if (showLegend) {
    basePlot <- basePlot + theme(legend.position="top");
  } else {
    basePlot <- basePlot + theme(legend.position="none");
  }
  return(basePlot + xlab(xlab) + ylab('Density'));
}
