ggConfidenceCurve <- function(metric = 'd', value = NULL, n = NULL,
                              conf.level=NULL,
                              wRange = c(.05, .8),
                              curveSize = 1,
                              curveColor = 'black',
                              confRange = c(.0001, .9999),
                              confLines = c(.50, .80, .95, .99),
                              widthLines = c(min(wRange), .1, .2, .3, max(wRange)),
                              lineColor = brewer.pal(9, 'Set1'),
                              lineSize=1,
                              lineAlpha = .5,
                              xlab = metric,
                              steps=1000,
                              theme=theme_bw(),
                              gradient=NULL,
                              gradientWidth=.01,
                              outputFile = NULL,
                              outputWidth = 16,
                              outputHeight = 16,
                              ggsaveParams = list(units='cm',
                                                  dpi=300,
                                                  type="cairo")) {
  ### Check arguments
  if (is.null(conf.level) && is.null(n)) {
    stop("Arguments 'conf.level' and 'n' cannot both be NULL!");
  }
  if (is.null(value)) {
    stop("You must always specify a value for your metric.");
  }
  if (tolower(metric) == 'd') {
    if (is.null(n)) {
      ### So, sample sizes on the y axis, and values on the x axis.
      ### So we need the values (bounds) corresponding to different
      ### widths (which then correspond to sample sizes).
      minN <- pwr.cohensdCI(d=value, w=max(wRange));
      maxN <- pwr.cohensdCI(d=value, w=min(wRange));
      df1 <- df2 <- data.frame(n = seq(minN, maxN, length.out=steps));
      confInts <- cohensdCI(d=value,
                            n=seq(minN, maxN, length.out=steps),
                            conf.level=conf.level);
      df1 <- cbind(df1, confInts[, 1]);
      df2 <- cbind(df2, confInts[, 2]);
    } else {
      ### So, confidence is on the y axis, and values on the x axis.
      ### So we need to the confidence interval bounds for the given
      ### sample size, for different confidence levels.
      minConf <- min(confRange);
      maxConf <- max(confRange);
      df1 <- df2 <- data.frame(confidence = seq(minConf, maxConf, length.out=steps));
      confInts <- matrix(unlist(lapply(df1$confidence, cohensdCI, d=value,
                                       n=n)), ncol=2, byrow=TRUE);
      df1 <- cbind(df1, confInts[, 1]);
      df2 <- cbind(df2, confInts[, 2]);
      ### If need be, get metric values corresponding to confidence
      ### interval bounds
      if (!is.null(confLines) && (length(confLines) > 0)) {
        metricValues <- list();
        for (i in seq_along(confLines)) {
          metricValues[[i]] <- cohensdCI(d=value, n=n, conf.level=confLines[i]);
        }
      }      
    }
  } else if (tolower(metric)=='r') {
    if (is.null(n)) {
      ### So, sample sizes on the y axis, and values on the x axis.
      ### So we need the values (bounds) corresponding to different
      ### widths (which then correspond to sample sizes).
      minN <- pwr.confIntR(r=value, w=max(wRange));
      maxN <- pwr.confIntR(r=value, w=min(wRange));
      df1 <- df2 <- data.frame(n = seq(minN, maxN, length.out=steps));
      confInts <- matrix(unlist(lapply(df1$n, confIntR, r=value,
                                       conf.level=conf.level)),
                         ncol=2, byrow=TRUE);
      df1 <- cbind(df1, confInts[, 1]);
      df2 <- cbind(df2, confInts[, 2]);
    } else {
      ### So, confidence is on the y axis, and values on the x axis.
      ### So we need to the confidence interval bounds for the given
      ### sample size, for different confidence levels.
      minConf <- min(confRange);
      maxConf <- max(confRange);
      df1 <- df2 <- data.frame(confidence = seq(minConf, maxConf, length.out=steps));
      confInts <- matrix(unlist(lapply(df1$confidence, confIntR, r=value, N=n)),
                         ncol=2, byrow=TRUE);
      df1 <- cbind(df1, confInts[, 1]);
      df2 <- cbind(df2, confInts[, 2]);
      ### If need be, get metric values corresponding to confidence
      ### interval bounds
      if (!is.null(confLines) && (length(confLines) > 0)) {
        metricValues <- list();
        for (i in seq_along(confLines)) {
          metricValues[[i]] <- confIntR(r=value, N=n, conf.level=confLines[i]);
        }
      }
    }
  } else {
    stop("Sorry, metric '", metric, "' is not implemented.");
  }
  plot <- ggplot();
  if (is.null(n)) {
    ### Add lines to indicate margins of error ('half-widths'), if we need to
    if (!is.null(widthLines) && (length(widthLines) > 0)) {
      
      if (tolower(metric) == 'd') {
        ### Get the required sample sizes to obtain intervals of those widths
        ### given the specified sample size and confidence level.
        yValues <- sapply(widthLines, pwr.cohensdCI, d=value, conf.level=conf.level);
        ### Then get the metric values
        metricValues <- cohensdCI(d=value, conf.level=conf.level, n = yValues);
      } else if (tolower(metric) == 'r') {
        ### Get the required sample sizes to obtain intervals of those widths
        ### given the specified sample size and confidence level.
        yValues <- sapply(widthLines, pwr.confIntR, r=value, conf.level=conf.level);
        ### Then get the metric values
        metricValues <- confIntR(r=value, conf.level=conf.level, N = yValues);
      }

      metricValueLabels <- ifelseObj(tolower(metric) == 'r',
                                     formatR(metricValues),
                                     round(metricValues, 2));
      
      yValueLabels <- rev(paste0(yValues, " (MoE=", round(widthLines, 2), ", total width=",
                                 round(2*widthLines, 2), ")"));

      if (length(lineColor) < length(confLines)) {
        lineColor <- rep(lineColor, each=widthLines)[1:length(widthLines)];
      }

      for (i in seq_along(widthLines)) {
        plot <- plot +
          geom_hline(yintercept = yValues[i],
                     color=lineColor[i],
                     size=lineSize,
                     alpha=lineAlpha) +
          geom_vline(xintercept = metricValues[i, 1],
                     color=lineColor[i],
                     size=lineSize,
                     alpha=lineAlpha) +
          geom_vline(xintercept = metricValues[i, 2],
                     color=lineColor[i],
                     size=lineSize,
                     alpha=lineAlpha);
      }
    }
    
    names(df1) <- names(df2) <- c('n', metric);
    plot <- plot +
      geom_line(data=df1, aes_string(x=metric, y='n'),
                 color = curveColor, size = curveSize) +
      geom_line(data=df2, aes_string(x=metric, y='n'),
                color = curveColor, size = curveSize) +
      ggtitle(paste0("Inverse Confidence Curve for ",
                     metric, " = ", value, " and ", round(100*conf.level, 2), "% confidence"));
  } else {
    ### Add gradient, if desired
    if (!is.null(gradient)) {
      if (length(gradient) == 1 && isTrue(gradient)) {
        gradient <- c("black", "white")
      }
      if (!(length(gradient)==2)) {
        stop("If specifying a gradient, specify exactly two values!");
      } else {
        if (!all(areColors(gradient))) {
          stop("Not all values specified in 'gradient' are colors!");
        } else {
          names(df1) <- c('confidence', metric);
          names(df2) <- c('confidence', metric);
          tmpDf <- rbind(df1, df2);
          plot <- plot +
            geom_tile(data=tmpDf, aes_string(x=metric, y='confidence', fill='confidence'),
                      height=Inf, width=gradientWidth) +
            scale_fill_gradient2(low = gradient[2], mid = gradient[1], high = gradient[2], midpoint = 0);
        }
      }
    }
    ### Add lines to indicate confidence intervals, if we need to
    if (!is.null(confLines) && (length(confLines) > 0)) {
      metricValues <- round(sort(unlist(metricValues)), 2);
      metricValueLabels <-
        ifelseObj((tolower(metric) == 'r'),
                  noZero(metricValues),
                  metricValues);
      yValues <- confLines;
      yValueLabels <- confLines;
      if (length(lineColor) < length(confLines)) {
        lineColor <- rep(lineColor, each=confLines)[1:length(confLines)];
      }
      for (i in seq_along(confLines)) {
        plot <- plot +
          geom_hline(yintercept = confLines[i],
                     color=lineColor[length(confLines)+1-i],
                     size=lineSize,
                     alpha=lineAlpha) +
          geom_vline(xintercept = metricValues[i],
                     color=lineColor[i],
                     size=lineSize,
                     alpha=lineAlpha) +
          geom_vline(xintercept = metricValues[length(metricValues)+1-i],
                     color=lineColor[i],
                     size=lineSize,
                     alpha=lineAlpha);
      }
    }
    names(df1) <- names(df2) <- c('confidence', metric);
    df1 <- rbind(c(1, -Inf), df1);
    df2 <- rbind(df2, c(1, Inf));
    plot <- plot +
      geom_line(data=df1, aes_string(x=metric, y='confidence'),
                color = curveColor, size = curveSize) +
      geom_line(data=df2, aes_string(x=metric, y='confidence'),
                color = curveColor, size = curveSize) +
      ggtitle(paste0("Confidence Curve for ",
                     metric, " = ", value, " and n = ", n));
    
  }
  if ((!is.null(n) && !is.null(confLines) && (length(confLines) > 0))
       || (!is.null(conf.level) && !is.null(widthLines) && (length(widthLines) > 0))) {
    plot <- plot +
      scale_x_continuous(sec.axis = dup_axis(breaks=metricValues,
                                             labels=metricValueLabels,
                                             name="")) +
      scale_y_continuous(sec.axis = dup_axis(breaks=round(sort(unlist(yValues)), 2),
                                             labels=yValueLabels,
                                             name=""));
  }
  plot <- plot + theme + xlab(xlab);

  if (!is.null(outputFile)) {
    ggsaveParameters <- c(list(filename = outputFile,
                               plot = plot,
                               width = outputWidth,
                               height = outputHeight),
                          ggsaveParams);
    do.call(ggsave, ggsaveParameters);
  }
  
  return(plot);
}
