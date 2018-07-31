### XKCD styled plots
#require(xkcd)
#vignette("xkcd-intro")

### Histogram with dots
# http://stackoverflow.com/questions/16216312/how-to-plot-stacked-point-histograms-in-ggplot2-in-r


### Note: this is necessary to prevent Rcmd CHECK from throwing a note;
### otherwise it think these variables weren't defined yet.
# utils::globalVariables(c("y_density", "yMaxFromY"));

### Theme used for the plots
dlvTheme <- function(base_size = 11, base_family = "",
                     ...) {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # axis.title        = element_blank(),
      # axis.text         = element_text(colour="#000000", size = rel(0.8)),
      # axis.ticks        = element_line(colour = "black"),
      # axis.title        = element_blank(),
      # legend.text       = element_text(size = rel(0.6)),
      # legend.key        = element_rect(colour = "grey80"),
      # legend.position   = "top",
      # legend.direction  = "horizontal",
      # legend.key.size   = unit(6, "mm"),
      # panel.background  = element_rect(fill = "white", colour = NA),
      # panel.border      = element_rect(fill = NA, colour = "grey50"),
      # panel.grid.major  = element_line(colour = "grey90", size = 0.2),
      # panel.grid.minor  = element_line(colour = "grey98", size = 0.5),
      # panel.margin      = unit(c(.5), "cm"),
      ...
    )
}



#' dlvPlot
#' 
#' The dlvPlot function produces a dot-violin-line plot, and dlvTheme is the
#' default theme.
#' 
#' This function creates Dot Violin Line plots. One image says more than a
#' thousand words; I suggest you run the example :-)
#' 
#' @aliases dlvPlot dlvTheme
#' @param dat The dataframe containing x, y and z.
#' @param x Character value with the name of the predictor ('independent')
#' variable, must refer to a categorical variable (i.e. a factor).
#' @param y Character value with the name of the critetion ('dependent')
#' variable, must refer to a continuous variable (i.e. a numeric vector).
#' @param z Character value with the name of the moderator variable, must refer
#' to a categorical variable (i.e. a factor).
#' @param conf.level Confidence of confidence intervals.
#' @param jitter Logical value (i.e. TRUE or FALSE) whether or not to jitter
#' individual datapoints. Note that jitter cannot be combined with posDodge
#' (see below).
#' @param binnedDots Logical value indicating whether to use binning to display
#' the dots. Overrides jitter and dotsize.
#' @param binwidth Numeric value indicating how broadly to bin (larger values
#' is more binning, i.e. combining more dots into one big dot).
#' @param error Character value: "none", "lines" or "whiskers"; indicates
#' whether to show the confidence interval as lines with (whiskers) or without
#' (lines) horizontal whiskers or not at all (none)
#' @param dotsize Character value: "density" or "normal"; when "density", the
#' size of each dot corresponds to the density of the distribution at that
#' point.
#' @param singleColor The color to use when drawing one or more univariate
#' distributions (i.e. when no \code{z} is specified.
#' @param comparisonColors The colors to use when a \code{z} is specified. This
#' should be at least as many colors as \code{z} has levels. By default,
#' palette \code{Set1} from \code{\link{RColorBrewer}} is used.
#' @param densityDotBaseSize Numeric value indicating base size of dots when
#' their size corresponds to the density (bigger = larger dots).
#' @param normalDotBaseSize Numeric value indicating base size of dots when
#' their size is fixed (bigger = larger dots).
#' @param violinAlpha Numeric value indicating alpha value of violin layer (0 =
#' completely transparent, 1 = completely opaque).
#' @param dotAlpha Numeric value indicating alpha value of dot layer (0 =
#' completely transparent, 1 = completely opaque).
#' @param lineAlpha Numeric value indicating alpha value of the confidence
#' interval line layer (0 = completely transparent, 1 = completely opaque).
#' @param connectingLineAlpha Numeric value indicating alpha value of the layer
#' with the lines connecting the means (0 = completely transparent, 1 =
#' completely opaque).
#' @param meanDotSize Numeric value indicating the size of the dot used to
#' indicate the mean in the line layer.
#' @param posDodge Numeric value indicating the distance to dodge positions (0
#' for complete overlap).
#' @param errorType If the error is shown using lines, this argument indicates
#' Whether the errorbars should show the confidence interval
#' (\code{errorType='ci'}), the standard errors (\code{errorType='se'}), or
#' both (\code{errorType='both'}). In this last case, the standard error will
#' be wider than the confidence interval.
#' @param outputFile A file to which to save the plot.
#' @param outputWidth,outputHeight Width and height of saved plot (specified in
#' centimeters by default, see \code{ggsaveParams}).
#' @param ggsaveParams Parameters to pass to ggsave when saving the plot.
#' @param base_size,base_family,...  Passed on to the ggplot theme_grey()
#' function.
#' @return The behavior of this function depends on the arguments.
#' 
#' If no x and z are provided and y is a character value, dlvPlot produces a
#' univariate plot for the numerical y variable.
#' 
#' If no x and z are provided, and y is c character vector, dlvPlot produces
#' multiple Univariate plots, with variable names determining categories on
#' x-axis and with numerical y variables on y-axis
#' 
#' If both x and y are a character value, and no z is provided, dlvPlot
#' produces a bivariate plot where factor x determines categories on x-axis
#' with numerical variable y on the y-axis (roughly a line plot with a single
#' line)
#' 
#' Finally, if x, y and z are each a character value, dlvPlot produces
#' multivariate plot where factor x determines categories on x-axis, factor z
#' determines the different lines, and with the numerical y variable on the
#' y-axis
#' 
#' An object is returned with the following elements: \item{dat.raw}{Raw
#' datafile provided when calling dlvPlot} \item{dat}{Transformed (long)
#' datafile dlvPlot uses} \item{descr}{Dataframe with extracted descriptives
#' used to plot the mean and confidence intervals} \item{yRange}{The range of
#' the Y variable used to construct the plot} \item{plot}{The plot itself}
#' @keywords utilities
#' @examples
#' 
#' ### Note: the 'not run' is simply because running takes a lot of time,
#' ###       but these examples are all safe to run!
#' \dontrun{
#' ### Create simple dataset
#' dat <- data.frame(x1 = factor(rep(c(0,1), 20)),
#'                   x2 = factor(c(rep(0, 20), rep(1, 20))),
#'                   y=rep(c(4,5), 20) + rnorm(40));
#' ### Generate a simple dlvPlot of y
#' dlvPlot(dat, y='y');
#' ### Now add a predictor
#' dlvPlot(dat, x='x1', y='y');
#' ### And finally also a moderator:
#' dlvPlot(dat, x='x1', y='y', z='x2');
#' ### The number of datapoints might be a bit clearer if we jitter
#' dlvPlot(dat, x='x1', y='y', z='x2', jitter=TRUE);
#' ### Although just dodging the density-sized dots might work better
#' dlvPlot(dat, x='x1', y='y', z='x2', posDodge=.3);
#' }
#' 
#' @export dlvPlot
dlvPlot <- function(dat, x = NULL, y, z = NULL, conf.level = .95,
                    jitter = "FALSE", binnedDots = TRUE, binwidth=NULL,
                    error="lines", dotsize="density",
                    singleColor = "black",
                    comparisonColors = brewer.pal(8, 'Set1'),
                    densityDotBaseSize=3, normalDotBaseSize=1,
                    violinAlpha = .2, dotAlpha = .4,
                    lineAlpha = 1, connectingLineAlpha = 1,
                    meanDotSize=5, posDodge=0.2, errorType = "both",
                    outputFile = NULL,
                    outputWidth = 10,
                    outputHeight = 10,
                    ggsaveParams = list(units='cm',
                                        dpi=300,
                                        type="cairo")) {
  
  ### This function constructs a dot-line-violin plot.

  ### Create object to return results
  res <- list();
  
  ### Store data
  res$dat.raw <- dat;
  ### Remove irrelevant variables
  res$dat <- dat <- data.frame(dat[, c(x, y, z)]);
  
  ### Remove incomplete cases
  res$dat <- data.frame(dat[complete.cases(dat), ]);

  ### Replace names again
  names(dat) <- names(res$dat) <- c(x, y, z);
  
  if(!is.null(x) & !(is.factor(dat[, x]))) {
    warning("Error: variable x (', x,') is not of type factor. X must be a categorical ",
            "variable with a limited number of categories. If this is the case, but it's ",
            "simply stored as a numerical vector, use the 'factor' function to convert ",
            "it (see '?factor'). Trying to convert x myself now.");
    res$dat[[x]] <- factor(res$dat[[x]]);
  }

  if(!is.null(z) & !(is.factor(dat[, z]))) {
    warning("Error: variable z (', z,') is not of type factor. Z must be a categorical ",
            "variable with a limited number of categories. If this is the case, but it's ",
            "simply stored as a numerical vector, use the 'factor' function to convert ",
            "it (see '?factor'). Trying to convert z myself now.");
    res$dat[[z]] <- factor(res$dat[[z]]);
  }
  
  if(is.null(x)) {
    ### We have no predictor variable - this means we construct univariate plots.

    ### Now check whether we have to construct one or several.
    if(length(y)==1) {
      
      ###############################################################
      ### Constructing one univariate plot                        ###
      ###############################################################
      
      ### Store variable name in dataframe
      if (is.null(res$dat$variable)) {
        res$dat$variable <- y;
        xVarName <- 'variable';
      }
      else {
        res$dat$variable_dlvPlot <- y;
        xVarName <- 'variable_dlvPlot';
      }
      
      ### Store density at y value
      dens <- density(res$dat[[y]], na.rm=TRUE);
      res$dat$y_density <- approx(dens$x, dens$y, xout=res$dat[[y]])$y;
      ### Multiply so that points at average density have size 1
      res$dat$y_density <- res$dat$y_density *
        (densityDotBaseSize/mean(res$dat$y_density, na.rm=TRUE));
      
      ### Construct dataframe with confidence interval info
      n <- nrow(res$dat);
      mean <- mean(res$dat[, y]);
      sd <- sd(res$dat[, y]);
      se <- sd / sqrt(nrow(res$dat));
      criticalValue <- qt(1-((1-conf.level)/2), df=n-1); 
      ci.lo <- mean - criticalValue * se;
      ci.hi <- mean + criticalValue * se;
      meanMinSE <- mean - se;
      meanPlusSE <- mean + se;
      res$descr <- data.frame(y = y,
                              n = n,
                              mean = mean, sd = sd,
                              se = se,
                              ci.lo = ci.lo,
                              ci.hi = ci.hi,
                              meanMinSE = meanMinSE,
                              meanPlusSE = meanPlusSE);
      res$yRange=c(min(res$dat[[y]][!is.na(res$dat[[y]])]),
                   max(res$dat[[y]][!is.na(res$dat[[y]])]));

      ### Generate plot
      res$plot <- ggplot(data=res$dat, aes_string(x=xVarName, y=y));
      res$plot <- res$plot + dlvTheme();
      res$plot <- res$plot + geom_violin(trim=FALSE, alpha=violinAlpha, fill=singleColor, linetype="blank");
      if (jitter) {
        res$plot <- res$plot + geom_jitter(position=position_jitter(width=.1, height=.01), alpha=dotAlpha);
      }
      else {
        if (binnedDots) {
          tempBinwidth <- ifelse(is.null(binwidth), (res$yRange[2]-res$yRange[1])/30, binwidth);
          res$plot <- res$plot + geom_dotplot(alpha=dotAlpha, show.legend=FALSE,
                                              binaxis="y", binwidth=tempBinwidth,
                                              dotsize=normalDotBaseSize,
                                              stackdir="center",
                                              color=singleColor,
                                              fill=singleColor,
                                              position=position_dodge(width=posDodge));
        }
        else if (dotsize=="density") {
          res$plot <- res$plot + geom_point(aes_string(size='y_density'), color=singleColor,
                                            alpha=dotAlpha, show.legend=FALSE);
        }
        else {
          res$plot <- res$plot + geom_point(alpha=dotAlpha, dotsize=normalDotBaseSize);
        }
      }
      if (error == "lines") {
        if (errorType=="ci") {
          res$plot <- res$plot + geom_pointrange(data=res$descr,
                                                 aes_string(x='y', y='mean', ymin='ci.lo', ymax='ci.hi'),
                                                 color=singleColor,
                                                 size = 1, alpha=lineAlpha);
        } else if (errorType=="se") {
          res$plot <- res$plot + geom_pointrange(data=res$descr,
                                                 aes_string(x='y', y='mean', ymin='meanMinSE', ymax='meanPlusSE'),
                                                 color=singleColor,
                                                 size = 1, alpha=lineAlpha);
        } else if (errorType=="both") {
          res$plot <- res$plot + geom_pointrange(data=res$descr,
                                                 aes_string(x='y', y='mean', ymin='ci.lo', ymax='ci.hi'),
                                                 size = 1, alpha=lineAlpha);
          res$plot <- res$plot + geom_errorbar(data=res$descr,
                                               aes_string(x='y', ymin='meanMinSE', ymax='meanPlusSE'),
                                               color=singleColor,
                                               size = 2, alpha=lineAlpha, width=0,
                                               inherit.aes = FALSE);
        }        
      }
      else if (error == "whiskers") {
        res$plot <- res$plot + geom_errorbar(data=res$descr,
                                             aes_string(x='y', y='mean', ymin='ci.lo', ymax='ci.hi'),
                                             color=singleColor,
                                             size = 1, width=.1, alpha=lineAlpha);
      }
      res$plot <- res$plot + geom_point(data=res$descr,
                                        color=singleColor,
                                        aes_string(x='y', y='mean'),
                                        size=meanDotSize,
                                        alpha=lineAlpha);
      
    }
    else {
      
      ###############################################################
      ### Constructing several univariate plots                   ###
      ###############################################################
      
      ### Apparently, we have to construct several plots.
      ### First generate a dataframe where the variables names
      ### are stored in another variable that we can use to
      ### make categories on the x axis
      
      ### Store original dataframe
      res$dat.original <- res$dat;
      res$dat <- data.frame();
      ### Create empty descriptives dataframe
      res$descr <- data.frame();
      
      ### Loop through original dataframe and construct new one
      for (currentVar in y) {
        tempDf <- data.frame(y = res$dat.original[, currentVar]);
        tempDf$x <-  currentVar;
        ### Store density for at y value
        dens <- density(tempDf$y, na.rm=TRUE);
        tempDf$y_density <- approx(dens$x, dens$y, xout=tempDf$y)$y;
        tempDf$y_density <- tempDf$y_density *
          (densityDotBaseSize/mean(tempDf$y_density, na.rm=TRUE));
        ### Store y values and name of y variable in res$dat dataframe
        res$dat <- rbind(res$dat, tempDf);
        ### Get mean and confidence interval for descriptives table
        n <- nrow(tempDf);
        mean <- mean(tempDf$y);
        sd <- sd(tempDf$y);
        se <- sd / sqrt(nrow(tempDf));
        criticalValue <- qt(1-((1-conf.level)/2), df=n-1); 
        ci.lo <- mean - criticalValue * se;
        ci.hi <- mean + criticalValue * se;
        meanMinSE <- mean - se;
        meanPlusSE <- mean + se;
        ### Add descriptives
        res$descr <- rbind(res$descr, data.frame(y = currentVar,
                                                 n = n,
                                                 mean = mean, sd = sd,
                                                 se = se,
                                                 ci.lo = ci.lo,
                                                 ci.hi = ci.hi,
                                                 meanMinSE = meanMinSE,
                                                 meanPlusSE = meanPlusSE));
      }
      
      res$yRange=c(min(res$dat[['y']][!is.na(res$dat[['y']])]),
                   max(res$dat[['y']][!is.na(res$dat[['y']])]));
      
      res$plot <- ggplot(data=res$dat, aes(x=x, y=y));
      res$plot <- res$plot + dlvTheme();
      res$plot <- res$plot + geom_violin(trim=FALSE, alpha=violinAlpha, fill=singleColor, linetype="blank");
      if (jitter) {
        res$plot <- res$plot + geom_jitter(position=position_jitter(width=.1, height=.01),
                                           color=singleColor,
                                           alpha=dotAlpha);
      }
      else {
        if (binnedDots) {
          tempBinwidth <- ifelse(is.null(binwidth), (res$yRange[2]-res$yRange[1])/30, binwidth);
          res$plot <- res$plot + geom_dotplot(alpha=dotAlpha, show.legend=FALSE,
                                              binaxis="y", binwidth=tempBinwidth, dotsize=normalDotBaseSize,
                                              color=singleColor,
                                              fill=singleColor,
                                              stackdir="center", position=position_dodge(width=posDodge)
                                              );
        }
        else if (dotsize=="density") {
          res$plot <- res$plot + geom_point(aes_string(size='y_density'), color=singleColor,
                                            alpha=dotAlpha, show.legend=FALSE);
        }
        else {
          res$plot <- res$plot + geom_point(alpha=dotAlpha,
                                            color=singleColor,
                                            dotsize=normalDotBaseSize);
        }
      }
      if (error == "lines") {
        if (errorType=="ci") {
          res$plot <- res$plot + geom_pointrange(data=res$descr,
                                                 aes_string(x='x', y='mean', ymin='ci.lo', ymax='ci.hi'),
                                                 color=singleColor,
                                                 size = 1, alpha=lineAlpha);
        } else if (errorType=="se") {
          res$plot <- res$plot + geom_pointrange(data=res$descr,
                                                 aes_string(x=y, y='mean', ymin='meanMinSE', ymax='meanPlusSE'),
                                                 color=singleColor,
                                                 size = 1, alpha=lineAlpha);
        } else if (errorType=="both") {
          res$plot <- res$plot + geom_pointrange(data=res$descr,
                                                 aes_string(x=y, y='mean', ymin='ci.lo', ymax='ci.hi'),
                                                 color=singleColor,
                                                 size = 1, alpha=lineAlpha);
          res$plot <- res$plot + geom_errorbar(data=res$descr,
                                               aes_string(x=y, y='mean', ymin='meanMinSE', ymax='meanPlusSE'),
                                               color=singleColor,
                                               size = 2, alpha=lineAlpha, width=0);
        }
      }
      else if (error == "whiskers") {
        res$plot <- res$plot + geom_errorbar(data=res$descr,
                                             aes(x=y, y=mean, ymin=ci.lo, ymax=ci.hi),
                                             color=singleColor,
                                             size = 1, width=.1, alpha=lineAlpha);
      }
      res$plot <- res$plot + geom_point(data=res$descr,
                                        aes(x=y, y=mean),
                                        color=singleColor,
                                        size=meanDotSize,
                                        alpha=lineAlpha);
      
    }
  }
  else {
    ### We have a predictor variable, so check whether we have a moderator
    if (is.null(z)) {
      
      ###############################################################
      ### Constructing multivariate plot without moderator        ###
      ###############################################################
      
      ### Construct dataframe with confidence interval info
      res$descr <- ddply(.data = res$dat, .variables = c(x),
                         .fun = function (dat, conf.level) {
                           dat <- dat[complete.cases(dat), ];
                           n <- nrow(dat);
                           mean <- mean(dat[, y]);
                           sd <- sd(dat[, y]);
                           se <- sd / sqrt(nrow(dat));
                           criticalValue <- qt(1-((1-conf.level)/2), df=n-1); 
                           ci.lo <- mean - criticalValue * se;
                           ci.hi <- mean + criticalValue * se;
                           meanMinSE <- mean - se;
                           meanPlusSE <- mean + se;
                           rslt <- data.frame(x = dat[1, x],
                                              y = y,
                                              n = nrow(dat),
                                              mean = mean, sd = sd,
                                              se = se, ci.lo = ci.lo,
                                              ci.hi = ci.hi,
                                              meanMinSE = meanMinSE,
                                              meanPlusSE = meanPlusSE,
                                              numericX = as.numeric(dat[1, x]));
                           rslt <- rslt[complete.cases(rslt), ];
                           return(rslt);
                         }, conf.level=conf.level);
      ### Store densities; must be done for each group (value of x)
      ### separately
      res$dat <- ddply(.data = res$dat, .variables = c(x),
                       .fun = function (dat) {
                         ### Store density for at y value
                         dens <- density(dat[[y]], na.rm=TRUE);
                         dat$y_density <- approx(dens$x, dens$y, xout=dat[[y]])$y;
                         ### Multiply with densityDotBaseSize / mean (this allows
                         ### control over the size of the dots)
                         dat$y_density <- dat$y_density *
                           (densityDotBaseSize/mean(dat$y_density, na.rm=TRUE));
                         return(dat);
                       });
      
      res$yRange=c(min(res$dat[[y]][!is.na(res$dat[[y]])]),
                   max(res$dat[[y]][!is.na(res$dat[[y]])]));

      res$plot <- ggplot(data=res$dat, aes_string(x=x, y=y));
      res$plot <- res$plot + dlvTheme();
      res$plot <- res$plot + geom_violin(trim=FALSE, alpha=violinAlpha,
                                         fill=singleColor, linetype="blank",
                                         position=position_dodge(width=posDodge));
      if (jitter) {
        res$plot <- res$plot + geom_jitter(position=position_jitter(width=.1, height=.01),
                                           color=singleColor,
                                           alpha=dotAlpha);
      }
      else {
        if (binnedDots) {
          tempBinwidth <- ifelse(is.null(binwidth), (res$yRange[2]-res$yRange[1])/30, binwidth);
          res$plot <- res$plot + geom_dotplot(alpha=dotAlpha, show.legend=FALSE,
                                              binaxis="y", binwidth=tempBinwidth,
                                              dotsize=normalDotBaseSize,
                                              color=singleColor,
                                              fill=singleColor,
                                              stackdir="center",
                                              position=position_dodge(width=posDodge));
        }
        else if (dotsize=="density") {
          res$plot <- res$plot + geom_point(aes_string(size='y_density'),
                                            color=singleColor,
                                            alpha=dotAlpha,
                                            show.legend=FALSE);
        }
        else {
          res$plot <- res$plot + geom_point(alpha=dotAlpha,
                                            color=singleColor,
                                            dotsize=normalDotBaseSize);
        }
      }
      if (error == "lines") {
        if (errorType=="ci") {
        res$plot <- res$plot + geom_pointrange(data=res$descr,
                                               aes_string(x='x', y='mean', ymin='ci.lo', ymax='ci.hi'),
                                               size = 1, alpha=lineAlpha,
                                               color=singleColor,
                                               inherit.aes = FALSE);
        } else if (errorType=="se") {
          res$plot <- res$plot + geom_pointrange(data=res$descr,
                                                 aes_string(x='x', y='mean', ymin='meanMinSE', ymax='meanPlusSE'),
                                                 size = 1, alpha=lineAlpha,
                                                 color=singleColor,
                                                 inherit.aes = FALSE);
        } else if (errorType=="both") {
          res$plot <- res$plot + geom_pointrange(data=res$descr,
                                                 aes_string(x='x', y='mean', ymin='ci.lo', ymax='ci.hi'),
                                                 size = 1, alpha=lineAlpha,
                                                 color=singleColor,
                                                 inherit.aes = FALSE);
          res$plot <- res$plot + geom_errorbar(data=res$descr,
                                               aes_string(x='x', ymin='meanMinSE', ymax='meanPlusSE'),
                                               size = 2, alpha=lineAlpha, width=0,
                                               color=singleColor,
                                               inherit.aes = FALSE);
        }
      }
      else if (error == "whiskers") {
        res$plot <- res$plot + geom_errorbar(data=res$descr,
                                             aes_string(x='x', y='mean', ymin='ci.lo', ymax='ci.hi'),
                                             size = 1, width=.1, alpha=lineAlpha,
                                             color=singleColor,
                                             inherit.aes = FALSE);
      }
      res$plot <- res$plot + stat_summary(fun.y=mean, geom="point",
                                          size=meanDotSize,
                                          color=singleColor,
                                          alpha=lineAlpha);
      res$plot <- res$plot + geom_line(data=res$descr,
                                       aes_string(x='x', y='mean', group=NA),
                                       color=singleColor,
                                       size=1, alpha=connectingLineAlpha);
    }
    else {
      
      ###############################################################
      ### Constructing multivariate plot with moderator           ###
      ###############################################################
      
      ### Construct dataframe with confidence interval info
      res$descr <- ddply(.data = res$dat, .variables = c(x, z),
                     .fun = function (dat, conf.level) {
                       dat <- dat[complete.cases(dat), ];
                       n <- nrow(dat);
                       mean <- mean(dat[, y]);
                       sd <- sd(dat[, y]);
                       se <- sd / sqrt(nrow(dat));
                       criticalValue <- qt(1-((1-conf.level)/2), df=n-1); 
                       ci.lo <- mean - criticalValue * se;
                       ci.hi <- mean + criticalValue * se;
                       meanMinSE <- mean - se;
                       meanPlusSE <- mean + se;
                       res <- data.frame(x = dat[1, x],
                                         y = y,
                                         z = dat[1, z],
                                         n = nrow(dat),
                                         mean = mean, sd = sd,
                                         se = se, ci.lo = ci.lo,
                                         ci.hi = ci.hi,
                                         meanMinSE = meanMinSE,
                                         meanPlusSE = meanPlusSE,
                                         numericX = as.numeric(dat[1, x]));
                       return(res[complete.cases(res), ]);
                     }, conf.level=conf.level);
      ### Store densities; must be done for each group (value of x)
      ### separately
      res$dat <- ddply(.data = res$dat, .variables = c(x, z),
                       .fun = function (dat) {
                         ### Store density for at y value
                         dens <- density(dat[[y]], na.rm=TRUE);
                         dat$y_density <- approx(dens$x, dens$y, xout=dat[[y]])$y;
                         ### Multiply with densityDotBaseSize / mean (this allows
                         ### control over the size of the dots)
                         dat$y_density <- dat$y_density *
                           (densityDotBaseSize/mean(dat$y_density, na.rm=TRUE));
                         return(dat);
                       });

      res$yRange=c(min(res$dat[[y]][!is.na(res$dat[[y]])]),
                   max(res$dat[[y]][!is.na(res$dat[[y]])]));
      
      res$plot <- ggplot(data=res$dat, aes_string(x=x, y=y, z=z,
                                                  colour=z,
                                                  fill=z,
                                                  group=paste0(x,":",z)));
      res$plot <- res$plot + dlvTheme();
      res$plot <- res$plot + geom_violin(data=res$dat, aes_string(fill=z),
                                         alpha=violinAlpha, trim=FALSE,
                                         linetype="blank",
                                         position=position_dodge(width=posDodge));
      if (jitter) {
        res$plot <- res$plot + geom_jitter(position=position_jitter(width=.1, height=.01),
                                           alpha=dotAlpha);
      }
      else {
        if (binnedDots) {
          tempBinwidth <- ifelse(is.null(binwidth),
                                 (res$yRange[2]-res$yRange[1])/30,
                                 binwidth);
          res$plot <- res$plot + geom_dotplot(alpha=dotAlpha, show.legend=FALSE,
                                              aes_string(fill=z), binaxis="y",
                                              binwidth=tempBinwidth,
                                              dotsize=normalDotBaseSize,
                                              stackdir="center",
                                              position=position_dodge(width=posDodge));
        }
        else if (dotsize=="density") {
          res$plot <- res$plot + geom_point(aes_string(size='y_density'),
                                            alpha=dotAlpha, show.legend=FALSE,
                                            position=position_dodge(width=posDodge));
        }
        else {
          res$plot <- res$plot + geom_point(alpha=dotAlpha, dotsize=normalDotBaseSize,
                                            position=position_dodge(width=posDodge));
        }
      }
      if (error == "lines") {
        if (errorType=="ci") {
          res$plot <- res$plot + geom_pointrange(data=res$descr,
                                                 aes_string(x='x', y='mean',
                                                            ymin='ci.lo',
                                                            ymax='ci.hi',
                                                            group='z',
                                                            color='z'),
                                                 size = 1, alpha=lineAlpha, position=position_dodge(width=posDodge),
                                                 inherit.aes = FALSE);
        } else if (errorType=="se") {
          res$plot <- res$plot + geom_pointrange(data=res$descr,
                                                 aes_string(x='x', y='mean',
                                                            ymin='meanMinSE',
                                                            ymax='meanPlusSE',
                                                            group='z',
                                                            color='z'),
                                                 size = 1, alpha=lineAlpha,
                                                 position=position_dodge(width=posDodge),
                                                 inherit.aes = FALSE);
        } else if (errorType=="both") {
          res$plot <- res$plot + geom_pointrange(data=res$descr,
                                                 aes_string(x='x', y='mean', ymin='ci.lo',
                                                            ymax='ci.hi', group='z',
                                                            color='z'),
                                                 size = 1, alpha=lineAlpha,
                                                 position=position_dodge(width=posDodge),
                                                 inherit.aes = FALSE);
          res$plot <- res$plot + geom_errorbar(data=res$descr,
                                               aes_string(x='x', ymin='meanMinSE',
                                                          ymax='meanPlusSE', group='z',
                                                          color='z'),
                                               size = 2, alpha=lineAlpha, width=0,
                                               position=position_dodge(width=posDodge),
                                               inherit.aes = FALSE);
        }
      }
      else if (error == "whiskers") {
        res$plot <- res$plot + geom_errorbar(data=res$descr,
                                             aes_string(x='x', ymin='ci.lo',
                                                        ymax='ci.hi', group='z',
                                                        color='z'),
                                             size = 1, width=.1,
                                             alpha=lineAlpha,
                                             position=position_dodge(width=posDodge),
                                             inherit.aes = FALSE);
      }
      res$plot <- res$plot + stat_summary(fun.y=mean, geom="point", size=meanDotSize, position=position_dodge(width=posDodge));
      res$plot <- res$plot + geom_line(data=res$descr, aes_string(x='x', y='mean', group='z'), size=1,
                                       alpha=connectingLineAlpha, position=position_dodge(width=posDodge));
      ### Add fill and color scales
      res$plot <- res$plot + scale_fill_manual(values=comparisonColors,
                                               name=z,
                                               labels=sort(unique(res$descr$z))) +
        scale_color_manual(values=comparisonColors,
                           name=z,
                           labels=sort(unique(res$descr$z)))
    }
  }
  
  assign('yMaxFromY', max(res$plot$data[, res$plot$labels$y]), envir = res$plot$plot_env);
  res$plot <- res$plot + aes_string(ymax = 'yMaxFromY');
  
  ### Set class of result
  class(res) <- c('dlvPlot');
  
  ### Save to a file, if desired
  if (!is.null(outputFile)) {
    ggsaveParameters <- c(list(filename = outputFile,
                               plot = res$plot,
                               width = outputWidth,
                               height = outputHeight),
                          ggsaveParams);
    do.call(ggsave, ggsaveParameters);
  }
  
  ### Return result
  return(res);
}

print.dlvPlot <- function(x, ...) {
  print(x$plot, ...);
  invisible();
}
