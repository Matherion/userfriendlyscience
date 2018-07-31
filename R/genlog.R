#' Generalized Logistic Analysis
#' 
#' This function implements the generalized logistic analysis introduced in
#' Verboon & Peters (2017). This analysis fits a logistic function (i.e. a
#' sigmoid) to a data series. This is useful when analysing single case
#' designs. The function enables easy customization of the main plot elements
#' and easy saving of the plot with anti-aliasing. \code{ggGenLogPlot} does
#' most of the plotting, and can be useful when trying to figure out sensible
#' starting and boundary/constraint values. \code{genlogCompleteStartValues}
#' tries to compute sensible starting and boundary/constraint values based on
#' the data.
#' 
#' For details, see Verboon & Peters (2017).
#' 
#' @aliases genlog genlogCompleteStartValues ggGenLogPlot
#' @param data The dataframe containing the variables for the analysis.
#' @param timeVar The name of the variable containing the measurement moments
#' (or an index of measurement moments). An index can also be specified, and
#' assumed to be 1 if omitted.
#' @param yVar The name of the dependent variable. An index can also be
#' specified, and assumed to be 2 if omitted.
#' @param phaseVar The variable containing the phase of each measurement. Note
#' that this normally should only have two possible values.
#' @param baselineMeasurements If no phaseVar is specified,
#' \code{baselineMeasurements} can be used to specify the number of baseline
#' measurements, which is then used to construct the \code{phaseVar} dummy
#' variable.
#' @param yRange This can be used to manually specify the possible values that
#' the dependent variable can take. If no \code{startBase} and \code{startTop}
#' are specified, the range of the dependent variable is used instead.
#' @param startInflection,startBase,startTop,startGrowthRate,startV The
#' starting values used when estimating the sigmoid using \code{minpack.lm}'s
#' \code{\link{nlsLM}} function. \code{startX} specifies the starting value to
#' use for the measurement moment when the change is fastest (i.e. the slope of
#' the sigmoid has the largest value); \code{startBase} and \code{startTop}
#' specify the starting values to use for the base (floor) and top (ceiling),
#' the plateaus of relative stability between which the sigmoid described the
#' shift; \code{startGrowthRate} specifies the starting value for the growth
#' rate; and \code{startV} specifies the starting value for the \emph{v}
#' parameter.
#' @param
#' inflectionPointBounds,growthRateBounds,baseMargin,topMargin,baseBounds,topBounds,vBounds
#' These values specify constraints to respect when estimating the parameters
#' of the sigmoid function using \code{minpack.lm}'s \code{\link{nlsLM}}.
#' \code{changeInitiationBounds} specifies between which values the initiation
#' of the shift must occur; \code{growthRateBounds} describes the bounds
#' constraining the possible values for the growth rate; \code{baseBounds} and
#' \code{topBounds} specify the constraints for possible values for the base
#' (floor) and top (ceiling), the plateaus of relative stability between which
#' the sigmoid described the shift; and if these are not specified,
#' \code{baseMargin} and \code{topMargin} are used in combination with the
#' range of the dependent variable to set these bounds (also see
#' \code{yRange}); and finally, \code{vBounds} specifies the possible values
#' that constrain the \emph{v} parameter.
#' @param changeDelay The number of measurements to add to the intervention
#' moment when setting the initial value for the inflection point.
#' @param colors The colors to use for the different plot elements.
#' @param alphas The alpha values (transparency, or rather, 'obliqueness', with
#' 0 indicating full transparency and 1 indicating full visibility) to use for
#' the different plot elements.
#' @param theme The theme to use in the plot.
#' @param pointSize,lineSize The sizes of points and lines in the plot.
#' @param yBreaks If \code{NULL}, the \code{\link{pretty}} function is used to
#' estimate the best breaks for the Y axis. If a value is supplied, this value
#' is used as the size of intervals between the (floored) minimum and
#' (ceilinged) maximum of \code{yRange} (e.g. if \code{yBreaks} is 1, a break
#' point every integer; if 2 and the minimum is 1 and the maximum is 7, breaks
#' at 1, 3, 5 and 7; etc).
#' @param initialValuesLineType The line type to use for the initial values; by
#' default set to \code{"blank"} for \code{genlog}, to hide them, and to
#' \code{"dashed"} for ggGenLogPlot.
#' @param curveSizeMultiplier A multiplyer for the curve size compared to the
#' other lines (e.g. specify '2' to have a curve of twice the size).
#' @param showPlot Whether to show the plot or not.
#' @param plotLabs A list with arguments to the \code{\link{ggplot2}}
#' \code{\link{labs}} function, which can be used to conveniently set plot
#' labels.
#' @param outputFile If not \code{NULL}, the path and filename specifying where
#' to save the plot.
#' @param outputWidth,outputHeight The dimensions of the plot when saving it
#' (in units specified in \code{ggsaveParams}).
#' @param ggsaveParams The parameters to use when saving the plot, passed on to
#' \code{\link{ggsave}}.
#' @param maxiter The maximum number of iterations used by \code{\link{nlsLM}}.
#' @return Mainly, this function prints its results, but it also returns them
#' in an object containing three lists: \item{input}{The arguments specified
#' when calling the function} \item{intermediate}{Intermediat objects and
#' values} \item{output}{The results such as the plot.}
#' @author Peter Verboon & Gjalt-Jorn Peters (both at the Open University of
#' the Netherlands)
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{genlogFunction}}
#' @references Verboon, P. & Peters, G.-J. Y. (2018) Applying the generalised
#' logistic model in single case designs: modelling treatment-induced shifts.
#' \emph{PsyArXiv} \url{https://doi.org/10.17605/osf.io/ad5eh}
#' @keywords hplot models htest
#' @examples
#' 
#' ### Load dataset
#' data(Singh);
#' 
#' ### Extract Jason
#' dat <- Singh[Singh$tier==1, ];
#' 
#' ### Conduct piecewise regression analysis
#' genlog(dat,
#'        timeVar='time',
#'        yVar='score_physical',
#'        phaseVar='phase');
#' 
#' 
#' @export genlog
genlog <- function(data,
                   timeVar = 1,
                   yVar = 2,
                   phaseVar = NULL,
                   baselineMeasurements = NULL, ### Was nA
                   yRange = NULL,
                   startInflection = NULL,      ### Was Xs
                   startBase = NULL,            ### Was ABs
                   startTop = NULL,             ### Was ATs
                   startGrowthRate = NULL,      ### Was Bs
                   startV = 1,
                   inflectionPointBounds = NULL,
                   growthRateBounds = c(-2, 2),
                   baseMargin = c(0, 3),
                   topMargin = c(-3, 0),
                   baseBounds = NULL,
                   topBounds = NULL,
                   vBounds = c(1, 1),
                   changeDelay = 4,
                   colors = list(bottomBound = viridis(4)[4],
                                 topBound = viridis(40)[37],
                                 curve = viridis(4)[3],
                                 mid = viridis(4)[2],
                                 intervention = viridis(4)[1],
                                 points = "black",
                                 outsideRange = "black"),
                   alphas = list(outsideRange = .2,
                                 bounds = 0,
                                 points = .5,
                                 mid = 0),
                   theme = theme_minimal(),
                   pointSize = 2,
                   lineSize = .5,
                   yBreaks = NULL,
                   initialValuesLineType = "blank",
                   curveSizeMultiplier = 2,
                   showPlot = TRUE,
                   plotLabs = NULL,
                   outputFile = NULL,
                   outputWidth = 16,
                   outputHeight = 16,
                   ggsaveParams = list(units='cm',
                                       dpi=300,
                                       type="cairo"),
                   maxiter = NULL) {

  res  <- list(input = as.list(environment()),
               intermediate = list(),
               output = list());

  ### Store names for easy access later on
  res$intermediate$yVarName <- yVar <-
    ifelse(is.numeric(yVar),
           names(data)[yVar],
           yVar);
  res$intermediate$timeVarName <- timeVar <-
    ifelse(is.numeric(timeVar),
           names(data)[timeVar],
           timeVar);
  res$intermediate$phaseVarName <- phaseVar <-
    ifelse(is.null(phaseVar),
           "none",
           ifelse(is.numeric(timeVar),
                  names(data)[phaseVar],
                  phaseVar));
  
  if (phaseVar=="none") {
    data <- data[, c(timeVar, yVar)];
  } else {
    data <- data[, c(timeVar, yVar, phaseVar)];
  }
  
  ### Remove cases with missing values
  res$intermediate$originalCases <- nrow(data);
  data <- data[complete.cases(data), ];
  res$intermediate$usedCases <- nrow(data);
  res$intermediate$omittedCases <-
    res$intermediate$originalCases - res$intermediate$usedCases;
  
  ### The definition of the generalized logistic function
  res$intermediate$GLF <- GLF <-
    paste0(yVar, " ~ base + (top - base)/ (1 + exp(-growthRate*(", timeVar, " - inflectionPoint))) ^ (1/v)");
  ### paste0(yVar, " ~ Ab + (At - Ab)/ (1 + exp(-B*(", timeVar, " - x0))) ^ (1/v)");
  ### "y ~ Ab + (At - Ab)/ (1 + exp(-B*(x-x0)))**(1/v)";

  ### If the time variable is actually provided as time instead of as
  ### indices/ranks, convert to numeric first.
  if (!is.numeric(data[, timeVar])) {
    if (any(class(data[, timeVar]) %in% c('Date', 'POSIXct', 'POSIXt', 'POSIXt'))) {
      res$intermediate$day0 <- min(data[, timeVar], na.rm=TRUE);
      res$intermediate$day0.formatted <- as.character(res$intermediate$day0);
      ### Compute number of days since first measurement
      data[, timeVar] <- (as.numeric(data[, timeVar]) - as.numeric(res$intermediate$day0)) / 86400;
    } else {
      stop("The timeVar variable does not have a class I can work with (numeric or date): instead it has class ",
           vecTxtQ(class(data[, timeVar])), ".");
    }
  }

  ### Number of measurements in pre-intervention phase
  if (is.null(baselineMeasurements) && is.null(phaseVar)) {
    stop("Provide number of measurements in pre-intervention phase, either by ",
         "specifying the variable indicating the phase in 'phaseVar', or by ",
         "specifying the number of baseline measurements in 'baselineMeasurements'.");
  } else {
    res$intermediate$baselineMeasurements <-
      baselineMeasurements <-
      ifelse(is.null(baselineMeasurements),
             sum(data[, phaseVar] == min(data[, phaseVar])),
             baselineMeasurements);
  }

  ### Get starting values if they're not provided yet
  res$intermediate$completeStartValues <-
    genlogCompleteStartValues(data,
                              timeVar = timeVar,
                              yVar = yVar,
                              phaseVar = phaseVar,
                              baselineMeasurements = baselineMeasurements,
                              yRange = yRange,
                              startInflection = startInflection,
                              startBase = startBase,
                              startTop = startTop,
                              startGrowthRate = startGrowthRate,
                              startV = startV,
                              changeDelay = changeDelay,
                              inflectionPointBounds = inflectionPointBounds,
                              growthRateBounds = growthRateBounds,
                              baseMargin = baseMargin,
                              topMargin = topMargin,
                              baseBounds = baseBounds,
                              topBounds = topBounds,
                              vBounds = vBounds,
                              returnFullObject = TRUE);
  
  #print(res$intermediate$completeStartValues);
  
  ### Merge with object with intermediate values
  res$intermediate <- c(res$intermediate,
                        res$intermediate$completeStartValues$intermediate,
                        res$intermediate$completeStartValues$output);
  
  yRange <- res$intermediate$yRange;

  if (!is.null(maxiter)) {
    nlsControl <- nls.control(maxiter=maxiter);
  } else {
    nlsControl <- nls.control();
  }
  
  ### Optimizing function
  tryCatch({
    out <- res$intermediate$nlsLM <-
      nlsLM(formula=GLF,
            data  = data,
            start = res$intermediate$startingValues,
            lower = res$intermediate$lowerBounds,
            upper = res$intermediate$upperBounds,
            control=nlsControl);
  }, error = function(e) {
    if (!is.null(res$intermediate$day0)) {
      res$intermediate$startingValues[1]  <-
        as.character(as.POSIXct(86400*res$intermediate$startInflection,
                                origin = res$intermediate$day0));
      res$intermediate$lowerBounds[1] <-
        as.character(as.POSIXct(86400*res$intermediate$inflectionPointBounds[1],
                                origin = res$intermediate$day0));
      res$intermediate$upperBounds[1] <-
        as.character(as.POSIXct(86400*res$intermediate$inflectionPointBounds[2],
                                origin = res$intermediate$day0));
    }

    stop(paste0("\n\nWhile running genlog, an error in optimization ",
                "function nlsLM was caught. This may ",
                "imply that the initial values you supplied are wrong, ",
                "or that too few data points are available to estimate ",
                "the parameters. The error was:\n\n",
                e, "\nThe starting and [constraining values] used were:\n\n  ",
                paste0(paste0(c("Change initiation:    ",
                                "Growth rate:          ",
                                "Curve bottom (floor): ",
                                "Curve top (ceiling):  ",
                                "V:                    "),
                              res$intermediate$startingValues, " [",
                              res$intermediate$lowerBounds, "; ",
                              res$intermediate$upperBounds,
                              "]"), collapse="\n  "),
                "\n\nTo visualise the data with these starting values and constraints, use the ",
                "ggGenLogPlot() function with exactly the same arguments."));
  });

  ### Extract coefficients
  res$output$inflectionPoint <- inflectionPoint <-
    x0 <- as.numeric(coef(out)[1]);
  res$output$growthRate <-
    B <-  as.numeric(coef(out)[2]);
  res$output$base <-
    Ab <- as.numeric(coef(out)[3]);
  res$output$top <-
    At <- as.numeric(coef(out)[4]);
  res$output$v <-
    v <-  as.numeric(coef(out)[5]);
  
  ### Compute effect size and fit information
  
  Dev <- deviance(out);
  validY <- !is.na(data[, yVar]);
  SSQtot <- sum((data[validY, yVar] - mean(data[validY, yVar])) ^ 2);
  res$output$deviance <- Dev;
  res$output$Rsq <-
    Rsq <- (SSQtot - Dev) / SSQtot;
  res$output$ES1 <- res$output$ESc <-
    ES1 <- (At -Ab) / sd(data[, yVar]);
  res$output$ES2 <- res$output$ESr <-
    ES2 <- (At -Ab) / diff(yRange);
  
  res$output$dat <-
    data.frame(deviance = Dev,
               Rsq = Rsq,
               ESc = ES1,
               ESr = ES2,
               growthRate = B,
               inflectionPoint = inflectionPoint,
               base = Ab,
               top = At);
  
  if (res$output$Rsq < 0) {
    warning(paste0("The fit of the generalized logistic function is *worse* ",
                   "than simply estimating the grand mean as best prediction ",
                   "for all observations (hence the negative R squared value). ",
                   "This suggests that the sigmoid model that the generalized ",
                   "logistic function attempts to fit may not be appropriate ",
                   "for these data, or that the initial or boundary values ",
                   "should be adjusted. Inspect the data closely."));
  }
  
  yfit <- genlogFunction(x = data[, timeVar],
                         x0 = inflectionPoint,
                         Ab = Ab,
                         At = At,
                         B = B,
                         v = v);

  interventionMoment <- mean(data[order(data[, timeVar],
                                        decreasing=FALSE)[c(baselineMeasurements,
                                                            baselineMeasurements+1)], timeVar]);
  
  if (!is.null(res$intermediate$day0)) {
    data[, timeVar] <-
      as.POSIXct(86400*data[, timeVar], origin = res$intermediate$day0);
    inflectionPoint <- as.POSIXct(86400*inflectionPoint,
                                  origin = res$intermediate$day0);
    interventionMoment <- as.POSIXct(86400*interventionMoment,
                                     origin = res$intermediate$day0);
  }
  
  if (is.null(plotLabs)) {
    plotLabs <- list(x = ifelse(is.null(res$intermediate$day0.formatted),
                                "Measurements",
                                "Date"),
                                #paste0("Days since ", res$intermediate$day0.formatted)),
                     y = yVar);
  }

  res$output$plot <-
    ggGenLogPlot(data,
                 timeVar = timeVar,
                 yVar = yVar,
                 phaseVar = phaseVar,
                 baselineMeasurements = baselineMeasurements,
                 ### These are all provided by genlogCompleteStartValues
                 yRange = res$intermediate$yRange,
                 startInflection = res$intermediate$startInflection,
                 startBase = res$intermediate$startBase,
                 startTop = res$intermediate$startTop,
                 startGrowthRate = res$intermediate$startGrowthRate,
                 startV = res$intermediate$startV,
                 changeDelay = changeDelay,
                 inflectionPointBounds = res$intermediate$inflectionPointBounds,
                 growthRateBounds = res$intermediategrowthRateBounds,
                 baseBounds = res$intermediatebaseBounds,
                 topBounds = res$intermediatetopBounds,
                 vBounds = res$intermediatevBounds,
                 ### These are specified when calling this function
                 colors = colors,
                 alphas = alphas,
                 theme = theme,
                 pointSize = pointSize,
                 lineSize = lineSize,
                 initialValuesLineType = initialValuesLineType,
                 yBreaks=yBreaks,
                 curveSizeMultiplier = curveSizeMultiplier,
                 plotLabs = plotLabs);
  
  res$output$plot <-
    res$output$plot +
    ### Add pre stable value (bottom/floor)
    geom_hline(yintercept=Ab,
               colour=colors$bottomBound,
               size=lineSize) +
    ### Add post stable value (top/ceiling)
    geom_hline(yintercept=At,
               colour=colors$topBound,
               size=lineSize) +
    ### Add moment of max change
    geom_vline(xintercept=inflectionPoint,
               colour=colors$mid,
               size=lineSize) +
    ### Add sigmoid
    geom_line(data=data.frame(x=data[, timeVar], y=yfit),
              aes_string(x='x', y='y'),
              colour=colors$curve,
              size = lineSize * curveSizeMultiplier);
    
  if (!is.null(res$intermediate$day0)) {
    res$output$inflectionPoint.numeric <-
      res$output$inflectionPoint;
    res$output$inflectionPoint <-
      as.POSIXct(86400*res$output$inflectionPoint,
                 origin = res$intermediate$day0);
  }
  
  if (!is.null(outputFile)) {
    ggsaveParameters <- c(list(filename = outputFile,
                               plot = res$output$plot,
                               width = outputWidth,
                               height = outputHeight),
                          ggsaveParams);
    do.call(ggsave, ggsaveParameters);
  }
  
  class(res) <- "genlog";
  
  return(res);
  
}

print.genlog <- function(x, digits=3, ...) {
  if (x$input$showPlot) {
    grid.newpage();
    grid.draw(x$output$plot);
  }

  if (x$intermediate$omittedCases > 0) {
    sampleInfo <- paste0("(N = ",
                         x$intermediate$usedCases,
                         "; removed ",
                         x$intermediate$omittedCases,
                         " cases with missing values)");
  } else {
    sampleInfo <- paste0("(N = ",
                         x$intermediate$originalCases,
                         ")");
  }
  
  cat0("Generalized Logistic Analysis ", sampleInfo, "\n\n",
       "Estimated sigmoid association between ",
       x$intermediate$timeVarName, " and ",
       x$intermediate$yVarName, ".\n\n");
  
  addAsterisk <- function(input, intermediate, name) {
    if (length(intermediate[[name]]) == 1) {
      return(ifelse(isTRUE(input[[name]] == intermediate[[name]]),
                    intermediate[[name]],
                    paste0(round(intermediate[[name]], digits=digits), "*")));
    } else {
      return(ifelse(isTRUE(input[[name]] == intermediate[[name]]),
                    paste0(formatCI(intermediate[[name]], digits=digits)),
                    paste0(formatCI(intermediate[[name]], digits=digits), "*")));
    }
  }
  
  startValueVars <- c('startInflection',
                      'startBase',
                      'startGrowthRate',
                      'startTop');
  startValues <- lapply(startValueVars,
                        addAsterisk,
                        input = x$input,
                        intermediate = x$intermediate);
  names(startValues) <- startValueVars;
  
  boundValueVars <- c('inflectionPointBounds',
                      'baseBounds',
                      'topBounds');
  boundValues <- lapply(boundValueVars,
                        addAsterisk,
                        input = x$input,
                        intermediate = x$intermediate);
  names(boundValues) <- boundValueVars;
  
  ### If we worked with dates, overwrite numeric value with pretty date
  if (!is.numeric(x$output$inflectionPoint)) {
    startValues$startInflection <-
      as.character(as.POSIXct(86400*x$intermediate$startInflection,
                              origin = x$intermediate$day0));
    boundValues$inflectionPointBounds <-
      paste0("[",
             paste0(as.character(as.POSIXct(86400*x$intermediate$inflectionPointBounds,
                                            origin = x$intermediate$day0)), collapse="; "),
             "]");
    if (!isTRUE(x$input$startInflection == x$intermediate$startInflection)) {
      startValues$startInflection <-
        paste0(startValues$startInflection, "*");
      boundValues$inflectionPointBounds <-
        paste0(boundValues$inflectionPointBounds, "*");
    }
  }
  
  cat0("Parameter starting values [and constraints]:\n",
       "  Inflection point: ", startValues$startInflection,
       " ", boundValues$inflectionPointBounds, "\n",
       "  Curve base:       ", startValues$startBase,
       " ", boundValues$baseBounds, "\n",
       "  Growth rate:      ", startValues$startGrowthRate,
       " ", formatCI(x$input$growthRateBounds, digits=digits), "\n",
       "  Curve top:        ", startValues$startTop,
       " ", boundValues$topBounds, "\n",
       "  V parameter:      ", x$input$startV,
       " ", formatCI(x$input$vBounds, digits=digits), "\n",
       "\n",
       "Note: Asterisks (*) denote values that were not specified manually (but inferred by genlog).\n\n");
  
  if (x$output$growthRate < 0) {
    baseLabel <- "  Curve base (plateau after change):  ";
    topLabel <- "  Curve top (plateau before change):  ";
  } else {
    baseLabel <- "  Curve base (plateau before change): ";
    topLabel <- "  Curve top (plateau after change):   ";
  }
  
  cat0("Parameter estimates:\n\n",
       baseLabel,
       round(x$output$base, digits=digits), "\n",
       "  Growth rate:                        ",
       round(x$output$growthRate, digits=digits), "\n",
       "  Inflection point:                   ",
       ifelse(is.numeric(x$output$inflectionPoint),
              round(x$output$inflectionPoint, digits=digits),
              as.character(x$output$inflectionPoint)), "\n",
       topLabel,
       round(x$output$top, digits=digits),
       "\n\n");
  cat0("Model fit and effect sizes estimates:\n\n",
       "  Deviance:              ", round(x$output$deviance, digits=digits), "\n",
       "  R squared:             ", round(x$output$Rsq, digits=digits), "\n",
       "  ESc (Cohen's d-based): ", round(x$output$ESc, digits=digits), "\n",
       "  ESr (Range-based):     ", round(x$output$ESr, digits=digits), "\n");
}
