#' Visualising Numbers Needed for Change
#' 
#' These functions can be used to visualise Numbers Needed for Change.
#' \code{erDataSeq} is a helper function to generate an Event Rate Data
#' Sequence, and it uses \code{convert.threshold.to.er} and
#' \code{convert.er.to.threshold} to convert thresholds to event rates and vice
#' versa.
#' 
#' These functions are used by \code{\link{nnc}} to show the distributions, and
#' event rates. They probably won't be used much on their own.
#' 
#' @aliases ggNNC erDataSeq convert.threshold.to.er convert.er.to.threshold
#' @param er Event rate to visualise (or convert).
#' @param threshold If the event rate is not available, a threshold value can
#' be specified instead, which is then used in conjunction with the mean
#' (\code{mean}) and standard deviation (\code{sd}) and assuming a normal
#' distribution to compute the event rate.
#' @param mean The mean of the control group distribution.
#' @param sd The standard deviation (of the control distribution, but assumed
#' to be the same for both distributions).
#' @param eventIfHigher Whether scores above or below the threshold are
#' considered 'an event'.
#' @param pRange The range of probabilities for which to so the distribution.
#' @param xStep Precision of the drawn distribution; higher values mean lower
#' precision/granularity/resolution.
#' @param cerDataSeq The \code{cerDataSeq} object.
#' @param d The value of Cohen's \emph{d}.
#' @param eventDesirable Whether an event is desirable or undesirable.
#' @param r The correlation between the determinant and behavior (for mediated
#' NNC's).
#' @param xlab The label to display for the X axis.
#' @param plotTitle The title of the plot; either one character value, this
#' value if used; if two, they are considered a prefix and suffix to be
#' pre/appended to the NNC value.
#' @param theme The theme to use for the plot.
#' @param lineSize The thickness of the lines in the plot.
#' @param cerColor The color to use for the event rate portion of the control
#' group distribution.
#' @param eerColor The color to use for the event rate portion of the
#' experimental group distribution.
#' @param cerLineColor The line color to use for the control group
#' distribution.
#' @param eerLineColor The line color to use for the experimental group
#' distribution.
#' @param dArrowColor The color of the arrow to show the effect size.
#' @param cerAlpha The alpha value (transparency) to use for the control group
#' distribution.
#' @param eerAlpha The alpha value (transparency) to use for the control group
#' distribution.
#' @param xLim This can be used to manually specify the limits for the X axis;
#' if \code{NULL}, sensible limits will be derived using
#' \code{xLimAutoDensityTolerance}.
#' @param xLimAutoDensityTolerance If \code{xLim} is \code{NULL}, the limits
#' will be set where the density falls below this proportion of its maximum
#' value.
#' @param showLegend Whether to show the legend (only if showing two
#' distributions).
#' @param verticalLineColor The color of the vertical line used to indicate the
#' threshold.
#' @param desirableColor The color for the desirable portion of the X axis.
#' @param desirableAlpha The alpha for the desirable portion of the X axis.
#' @param undesirableColor The color for the undesirable portion of the X axis.
#' @param undesirableAlpha The color for the undesirable portion of the X axis.
#' @param desirableTextColor The color for the text to indicate the desirable
#' portion of the X axis.
#' @param undesirableTextColor The color for the text to indicate the
#' undesirable portion of the X axis.
#' @param dArrowDistance The distance of the effect size arrow from the top of
#' the distributions.
#' @param dLabelDistance The distance of the effect size label from the top of
#' the distributions.
#' @param pdist,qdist Distributions to use when converting thresholds to event
#' rates and vice versa; defaults to the normal distribution.
#' @return \code{erDataSeq} returns a data sequence; \code{ggNNC} a
#' \code{\link{ggplot}}.
#' @author Gjalt-Jorn Peters & Stefan Gruijters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{nnc}}
#' @references Gruijters, S. L. K., & Peters, G.-J. Y. (2017). Introducing the
#' Numbers Needed for Change (NNC): A practical measure of effect size for
#' intervention research.
#' @keywords utilities
#' @examples
#' 
#' ### Show distribution for an event rate value of 125
#' ggNNC(erDataSeq(threshold=125, mean=90, sd=30));
#' 
#' ### If the event occurs under the threshold instead of
#' ### above it
#' ggNNC(erDataSeq(threshold=125, mean=90, sd=30,
#'       eventIfHigher = FALSE));
#' 
#' ### ... And for undesirable events (note how
#' ### desirability is an argument for ggNNC, whereas
#' ### whether an event occurs 'above' or 'below' the
#' ### threshold is an argument for erDataSeq):
#' ggNNC(erDataSeq(threshold=125, mean=90, sd=30,
#'                 eventIfHigher = FALSE),
#'       eventDesirable = FALSE);
#' 
#' ### Show event rate for both experimental and
#' ### control conditions, and show the numbers
#' ### needed for change
#' ggNNC(erDataSeq(threshold=125, mean=90, sd=30), d=.5);
#' 
#' ### Illustration of how even with very large effect
#' ### sizes, if the control event rate is very high,
#' ### you'll still need a high number of NNC
#' ggNNC(erDataSeq(er=.9), d=1);
#' 
#' 
#' @export ggNNC
ggNNC <- function(cerDataSeq, d = NULL,
                  eventDesirable = TRUE,
                  r = 1,
                  xlab = "Continuous outcome",
                  plotTitle = c("Numbers Needed for Change = ", ""),
                  theme=theme_bw(),
                  lineSize=1,
                  cerColor = '#EBF2F8',
                  eerColor = "#172F47", #'#CADDED',
                  cerLineColor = "#888888",
                  eerLineColor = "#000000",
                  dArrowColor = "#000000",
                  cerAlpha = .66,
                  eerAlpha = .66,
                  xLim = NULL,
                  xLimAutoDensityTolerance = .001,
                  showLegend = TRUE,
                  verticalLineColor = "#172F47",
                  desirableColor = "#00FF00",
                  desirableAlpha = .2,
                  undesirableColor = "#FF0000",
                  undesirableAlpha = .2,
                  desirableTextColor = "#009900",
                  undesirableTextColor = "#990000",
                  dArrowDistance = .04 * max(cerDataSeq$density),
                  dLabelDistance = .08 * max(cerDataSeq$density)) {

  if (!('erDataSeq' %in% class(cerDataSeq))) {
    stop("As 'erDataSeq', you must pass an object of class 'erDataSeq', such as ",
         "the result of a call to function 'erDataSeq' (see ?erDataSeq for help).");
  }

  eventIfHigher <- attr(cerDataSeq, 'eventIfHigher');
  
  cer <- attr(cerDataSeq, 'er');

  if (!is.null(d)) d <- convert.r.to.d(convert.d.to.r(d) * r);
  if (!is.null(d)) {
    eer <- convert.d.to.eer(d, cer,
                            eventDesirable=eventDesirable, eventIfHigher=eventIfHigher);
  } else {
    eer <- cer;
  }

  if (is.null(d)) d <- 0;

  sd <- attr(cerDataSeq, 'sd');
  cerValue <- attr(cerDataSeq, 'threshold');
  meanValue <- attr(cerDataSeq, 'mean');
  eerDataSeq <- cerDataSeq;
  eerDataSeq$x <- eerDataSeq$x + d * sd;
  newMeanValue <- meanValue + d * sd;
  cerValueDensity <- cerDataSeq[cerDataSeq$x == max(cerDataSeq[cerDataSeq$x < cerValue, 'x']), 'density'];
  eerValueDensity <- eerDataSeq[eerDataSeq$x == max(eerDataSeq[eerDataSeq$x < cerValue, 'x']), 'density'];
  cerLabel <- paste0("CER = ", round(100*cer, 2), ifelse(d != 0, "%    ", "%"));
  eerLabel <- paste0("EER = ", round(100*eer, 2), "%");
  nnc <- nnc(d = d, cer = cer,
             eventDesirable=eventDesirable, eventIfHigher=eventIfHigher,
             plot=FALSE);
  if (!is.null(plotTitle)) {
    if (length(plotTitle) == 2) {
      plotTitle <- paste0(plotTitle[1], round(nnc, 2), plotTitle[2]);
    } else {
      plotTitle <- paste0(plotTitle, collapse="");
    }
  }

  ### Compute sensible limits
  densityTolerance <- xLimAutoDensityTolerance * max(cerDataSeq$density);
  if (meanValue < newMeanValue) {
    lowestXWithDensity <- floor(max(cerDataSeq[cerDataSeq$density < densityTolerance & cerDataSeq$x < meanValue, 'x']));
    highestXWithDensity <- ceiling(min(eerDataSeq[eerDataSeq$density < densityTolerance & eerDataSeq$x > newMeanValue, 'x']));
  } else {
    lowestXWithDensity <- floor(max(eerDataSeq[eerDataSeq$density < densityTolerance & eerDataSeq$x < newMeanValue, 'x']));
    highestXWithDensity <- ceiling(min(cerDataSeq[cerDataSeq$density < densityTolerance & cerDataSeq$x > meanValue, 'x']));
  }
  if (is.null(xLim)) xLim <- c(lowestXWithDensity,
                               highestXWithDensity);

  ### Basic plot
  basePlot <- ggplot() + theme + xlim(xLim);

  ### Layer with CER normal curve
  if (eventIfHigher) {
    cerFill <- geom_ribbon(data = cerDataSeq[cerDataSeq$x > cerValue, ],
                           aes(x=x, ymax=density, ymin=0, fill=cerLabel), alpha=cerAlpha);
  } else  {
    cerFill <- geom_ribbon(data = cerDataSeq[cerDataSeq$x < cerValue, ],
                           aes(x=x, ymax=density, ymin=0, fill=cerLabel), alpha=cerAlpha);
  }
  ### Add line on top
  cerOutline <- geom_line(data=cerDataSeq, aes(x=x, y=density), size=lineSize, color=cerLineColor,
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
  eerOutline <- geom_line(data=eerDataSeq, aes(x=x, y=density), size=lineSize, color=eerLineColor,
                          na.rm=TRUE);
  ### Vertical line to show EER
  eerLine <- geom_segment(aes(x=cerValue, xend=cerValue, y=0, yend=eerValueDensity),
                          size=lineSize, color=verticalLineColor);

  ### Indicator for difference between distributions
  dArrow <- geom_segment(aes(x = meanValue, xend = newMeanValue,
                             y = max(cerDataSeq$density) + dArrowDistance,
                             yend = max(cerDataSeq$density) + dArrowDistance),
                         arrow=arrow(length = unit(.02, 'npc'), ends='last', type='closed', angle=20),
                         size=lineSize, color=dArrowColor);
  dText <- geom_text(aes(x = mean(c(meanValue, newMeanValue)),
                         y = max(cerDataSeq$density) + dLabelDistance),
                     hjust=.5, label=paste0("d = ", round(d, 2)));

  ### Convert desirable and undesirable to event and no event
  if (eventDesirable) {
    eventColor <- desirableColor;
    eventAlpha <- desirableAlpha;
    eventTextColor <- desirableTextColor;
    noEventColor <- undesirableColor;
    noEventAlpha <- undesirableAlpha;
    noEventTextColor <- undesirableTextColor;
  } else {
    eventColor <- undesirableColor;
    eventAlpha <- undesirableAlpha;
    eventTextColor <- undesirableTextColor;
    noEventColor <- desirableColor;
    noEventAlpha <- desirableAlpha;
    noEventTextColor <- desirableTextColor;
  }

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
  zeroLine <- geom_hline(aes(yintercept=0), color="#000000", size=lineSize);

  ### Build & return plot
  basePlot <- basePlot +
    eventBarNoEvent + eventBarEvent +
    eventBarNoEventText + eventBarEventText;
  if (d == 0) {
    basePlot <- basePlot + cerFill + cerOutline + cerLine + zeroLine +
      scale_fill_manual(values = c(cerColor), name="");
  } else if (d>0) {
    basePlot <- basePlot + eerFill + eerOutline + eerLine +
      cerFill + cerOutline + cerLine;
    basePlot <- basePlot + dArrow + dText + zeroLine +
      scale_fill_manual(values = c(cerColor, eerColor), name="");
  } else {
    basePlot <- basePlot + eerFill + eerOutline + eerLine +
      cerFill + cerOutline + cerLine;
    basePlot <- basePlot + dArrow + dText + zeroLine +
      scale_fill_manual(values = c(cerColor, eerColor), name="");
  }
  if (showLegend && d!=0) {
    basePlot <- basePlot +
      guides(fill=guide_legend(override.aes=list(color=c(cerLineColor, eerLineColor), size=lineSize))) +
      theme(legend.position="top");
  } else {
    basePlot <- basePlot + theme(legend.position="none");
  }
  if (!is.null(plotTitle) && (d!=0)) {
    basePlot <- basePlot + ggtitle(plotTitle);
  }
  return(basePlot + xlab(xlab) + ylab('Density'));
}
