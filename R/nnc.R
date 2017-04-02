nnc <- function(d = NULL, cer = NULL, r = 1,
                eventDesirable = TRUE, eventIfHigher = TRUE,
                d.ci = NULL, cer.ci = NULL, r.ci=NULL,
                d.n = NULL, cer.n = NULL, r.n = NULL, plot = TRUE,
                meanValue = 0, sd = 1,
                returnPlot = TRUE, silent=FALSE) {

  if (is.null(d)) {
    stop("You have to provide an estimate for Cohen's d (argument 'd'). If you do not have ",
         "a Cohen's d estimate, instead use convert.d.to.t (see ?convert.d.to.t for the ",
         "manual), e.g. provide:\n\n  nnc(d=convert.t.to.d(t = 3.2, df=98));\n\n",
         "Of course, replace '3.2' and '98' with your t value and the degrees of freedom.");
  }

  if (is.null(cer)) {
    if (!silent) {
      warning("You did not specify a Control Event Rate (CER, argument 'cer'). I will use ",
              "Kraemer & Kupfer's approach.");
    }
  } else {
    if (length(cer) > 1) {
      stop("When specifying a confidence interval for the CER, use argument 'cer.ci'!");
    }
  }

  if (length(d) > 1) {
    stop("When specifying a confidence interval for Cohen's d, use argument 'd.ci'!");
  }
  if (length(r) > 1) {
    stop("When specifying a confidence interval for the correlation, use argument 'r.ci'!");
  }

  if (!is.null(r.ci) && (r == 1)) r <- NULL;

  ### Compute confidence intervals if we can
  if (is.null(d.ci) && !is.null(d.n))
    d.ci <- cohensdCI(d=d, n = sum(d.n));
  if (is.null(cer.ci) && !is.null(cer.n))
    cer.ci <- prop.test(cer*cer.n, cer.n)$conf.int[1:2]
  if (is.null(r.ci) && !is.null(r.n))
    r.ci <- confIntR(r=r, N = r.n);

  ### Where we were unable to compute confidence intervals, just take the
  ### point estimate as both lower and upper bounds
  if (is.null(cer.ci)) cer.ci <- rep(cer, 2);
  if (is.null(d.ci)) d.ci <- rep(d, 2);
  if (is.null(r.ci)) r.ci <- rep(r, 2);

  ### Sort confidence intervals so that the value leading to the
  ### most conservative outcome is the highest

  ### Lower values are more conservative
  d.ci <- sort(d.ci);

  ### Higher values are more conservative
  r.ci <- sort(r.ci, decreasing=TRUE);

  ### Values closer to .5 are more conservative
  if (cer.ci[2] - .5 == min(abs(cer.ci - .5))) cer.ci <- rev(cer.ci);

  nnc.lb <- convert.d.to.nnc(d=d.ci[1], cer=cer.ci[1], r=r.ci[1],
                             eventDesirable=eventDesirable, eventIfHigher=eventIfHigher);
  nnc.ub <- convert.d.to.nnc(d=d.ci[2], cer=cer.ci[2], r=r.ci[2],
                             eventDesirable=eventDesirable, eventIfHigher=eventIfHigher);

  eer.ci <- c(attr(nnc.lb, 'eer'),
              attr(nnc.ub, 'eer'));

  nnc <- c(nnc.lb,
           nnc.ub);

  if (identical(nnc[1], nnc[2])) nnc <- nnc[1];

  res <- ceiling(nnc);

  attr(res, 'nnc.raw') <- nnc;
  attr(res, 'eventDesirable') <- eventDesirable;

  if (diff(range(cer.ci))) {
    attr(res, 'cer.ci') <- cer.ci;
  } else {
    attr(res, 'cer') <- cer.ci[1]
  }

  if (diff(range(eer.ci))) {
    attr(res, 'eer.ci') <- eer.ci;
  } else {
    attr(res, 'eer') <- eer.ci[1]
  }

  if (diff(range(r.ci))) {
    attr(res, 'r.ci') <- r.ci;
  } else {
    attr(res, 'r') <- r.ci[1]
  }

  if (diff(range(d.ci))) {
    attr(res, 'd.ci') <- d.ci;
  } else {
    attr(res, 'd') <- d.ci[1]
  }

  if (plot) {
    if (is.null(d)) {
      d <- mean(d.ci);
      if (!silent)
        cat0("Warning: no point estimate for Cohen's d supplied, so using the simple mean ",
             "of the lower and upper confidence interval bounds (", round(d, 2), ") for the plot!\n");
    }
    if (is.null(cer)) {
      cer <- mean(cer.ci);
      if (!silent)
        cat0("Warning: no point estimate for the CER supplied, so using the simple mean ",
             "of the lower and upper confidence interval bounds (", formatR(cer), ") for the plot!\n");
    }
    if (is.null(r)) {
      r <- mean(r.ci);
      if (!silent)
        cat0("Warning: no point estimate for the correlation supplied, so using the simple mean ",
             "of the lower and upper confidence interval bounds (", formatR(r), ") for the plot!\n");
    }

    plot <- ggNNC(erDataSeq(er=cer, meanValue=meanValue, sd=sd, eventIfHigher=eventIfHigher),
                  eventDesirable = eventDesirable,
                  d=d, r=r);
    if (returnPlot) {
      attr(res, 'plot') <- plot;
    } else {
      grid.newpage();
      grid.draw(plot);
    }
  }

  class(res) <- c('nnc', class(res));

  return(res);

}

print.nnc <- function(x, ...) {
  if (!is.null(attr(x, 'plot'))) {
    grid.newpage();
    grid.draw(attr(x, 'plot'));
  }

  if (is.null(attr(x, 'cer.ci'))) {
    cer <- attr(x, 'cer');
    cerStatement <- paste0("a Control Event Rate (CER) of ", formatR(cer));
  } else {
    cer <- formatCI(sort(attr(x, 'cer.ci')), noZero=TRUE);
    cerStatement <- paste0("a Control Event Rate (CER) with a confidence interval of ",
                           cer);
  }

  if (is.null(attr(x, 'eer.ci'))) {
    eer <- formatR(attr(x, 'eer'));
    eerStatement <- paste0(", an Experimental Event Rate (EER) of ", eer);
  } else {
    eer <- formatCI(sort(attr(x, 'eer.ci')), noZero=TRUE);
    eerStatement <- paste0(", an Experimental Event Rate (EER) with a confidence interval of ",
                           eer);
  }

  if (is.null(attr(x, 'd.ci'))) {
    d <- attr(x, 'd');
    dStatement <- paste0(" and a Cohen's d of ", d);
  } else {
    d <- formatCI(sort(attr(x, 'd.ci')));
    dStatement <- paste0(" and a Cohen's d with a confidence interval of ",
                         d);
  }

  if (is.null(attr(x, 'r.ci'))) {
    r <- attr(x, 'r');
    if (r < 1) {
      rStatement <- paste0(", and assuming a correlation of ", formatR(r),
                           " between the dependent measure and behavior");
    } else {
      rStatement <- "";
    }
  } else {
    r <- formatCI(sort(attr(x, 'r.ci')), noZero=TRUE);
    rStatement <- paste0(", and assuming a correlation with a confidence interval of ",
                         r,
                         " between the dependent measure and behavior");
  }

  if (length(x) > 1) {
    nnc <- formatCI(x);
  } else {
    nnc <- x;
  }

  cat0("\n",
       "Numbers Needed for Change: ", nnc, "\n\n",
       "(Based on ", cerStatement,
       eerStatement, dStatement, rStatement, ".)\n");
}
