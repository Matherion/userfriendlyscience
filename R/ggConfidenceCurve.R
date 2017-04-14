ggConfidenceCurve <- function(metric = 'd', value = .5, n = 128,
                              conf.level=NULL,
                              wRange = c(.05, 1),
                              confRange = c(.0001, .9999),
                              steps=1000,
                              theme=theme_bw()) {
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
      names(df1) <- names(df2) <- c('n', metric);
      plot <- ggplot() +
        geom_line(data=df1, aes_string(x=metric, y='n')) +
        geom_line(data=df2, aes_string(x=metric, y='n')) +
        ggtitle(paste0("Inverse Confidence Curve for ",
                       metric, " = ", value, " and conf.level = ", conf.level));
      
    } else {
      ### So, confidence is on the y axis, and values on the x axis.
      ### So we need to the confidence interval bounds for the given
      ### sample size, for different confidence levels.
      minConf <- min(confRange);
      maxConf <- max(confRange);
      df1 <- df2 <- data.frame(confidence = seq(minConf, maxConf, length.out=steps));
      confInts <- matrix(unlist(lapply(df1$confidence,
                                       cohensdCI,
                                       d=value,
                                       n=n)), ncol=2, byrow=TRUE);
      df1 <- cbind(df1, confInts[, 1]);
      df2 <- cbind(df2, confInts[, 2]);
      names(df1) <- names(df2) <- c('confidence', metric);
      plot <- ggplot() +
        geom_line(data=df1, aes_string(x=metric, y='confidence')) +
        geom_line(data=df2, aes_string(x=metric, y='confidence')) +
        ggtitle(paste0("Confidence Curve for ",
                       metric, " = ", value, " and n = ", n));
    }
  } else if (tolower(metric)=='r') {
    if (is.null(n)) {
      ### So, sample sizes on the y axis, and values on the x axis.
      ### So we need the values (bounds) corresponding to different
      ### widths (which then correspond to sample sizes).
      minN <- pwr.confIntR(r=value, w=max(wRange));
      maxN <- pwr.confIntR(r=value, w=min(wRange));
      df1 <- df2 <- data.frame(n = seq(minN, maxN, length.out=steps));
      confInts <- matrix(unlist(lapply(df1$n,
                                       confIntR,
                                       r=value,
                                       conf.level=conf.level)),
                         ncol=2, byrow=TRUE);
      df1 <- cbind(df1, confInts[, 1]);
      df2 <- cbind(df2, confInts[, 2]);
      names(df1) <- names(df2) <- c('n', metric);
      plot <- ggplot() +
        geom_line(data=df1, aes_string(x=metric, y='n')) +
        geom_line(data=df2, aes_string(x=metric, y='n')) +
        ggtitle(paste0("Inverse Confidence Curve for ",
                       metric, " = ", value, " and conf.level = ", conf.level));
      
    } else {
      ### So, confidence is on the y axis, and values on the x axis.
      ### So we need to the confidence interval bounds for the given
      ### sample size, for different confidence levels.
      minConf <- min(confRange);
      maxConf <- max(confRange);
      df1 <- df2 <- data.frame(confidence = seq(minConf, maxConf, length.out=steps));
      confInts <- matrix(unlist(lapply(df1$confidence,
                                       confIntR,
                                       r=value,
                                       N=n)),
                         ncol=2, byrow=TRUE);
      df1 <- cbind(df1, confInts[, 1]);
      df2 <- cbind(df2, confInts[, 2]);
      names(df1) <- names(df2) <- c('confidence', metric);
      plot <- ggplot() +
        geom_line(data=df1, aes_string(x=metric, y='confidence')) +
        geom_line(data=df2, aes_string(x=metric, y='confidence')) +
        ggtitle(paste0("Confidence Curve for ",
                       metric, " = ", value, " and n = ", n));
    }
  } else {
    stop("Sorry, metric '", metric, "' is not implemented.");
  }
  plot <- plot + theme;
  return(plot);
}
