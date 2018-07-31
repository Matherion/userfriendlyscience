#' oneway
#' 
#' The oneway function wraps a number of analysis of variance functions into
#' one convenient interface that is similar to the oneway anova command in
#' SPSS.
#' 
#' 
#' @param y y has to be a numeric vector.
#' @param x x has to be vector that either is a factor or can be converted into
#' one.
#' @param posthoc Which post-hoc tests to conduct. Valid values are any
#' correction methods in p.adjust.methods (at the time of writing of this
#' document, "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr",
#' "none"), as well as "tukey" and "games-howell".
#' @param means Whether to show the means for the y variable in each of the
#' groups determined by the x variable.
#' @param fullDescribe If TRUE, not only the means are shown, but all
#' statistics acquired through the 'describe' function in the 'psych' package
#' are shown.
#' @param levene Whether to show Levene's test for equality of variances (using
#' \code{car}'s \code{\link{leveneTest}} function but specifying
#' \code{\link{mean}} as function to compute the center of each group).
#' @param plot Whether to show a plot of the means of the y variable in each of
#' the groups determined by the x variable.
#' @param digits The number of digits to show in the output.
#' @param omegasq Whether to show the omega squared effect size.
#' @param etasq Whether to show the eta squared effect size (this is biased and
#' generally advised against; omega squared is less biased).
#' @param corrections Whether to show the corrections for unequal variances
#' (Welch and Brown-Forsythe).
#' @param pvalueDigits The number of digits to show for p-values; smaller
#' p-values will be shown as <.001 or <.0001 etc.
#' @param t Whether to transpose the dataframes with the means (if requested)
#' and the anova results. This can be useful for blind people.
#' @param conf.level Confidence level to use when computing the confidence
#' interval for eta^2. Note that the function we use doubles the 'unconfidence'
#' level to maintain consistency with the NHST value (see
#' http://yatani.jp/HCIstats/ANOVA#RCodeOneWay,
#' http://daniellakens.blogspot.nl/2014/06/calculating-confidence-intervals-for.html
#' or Steiger, J. H. (2004). Beyond the F test: Effect size confidence
#' intervals and tests of close fit in the analysis of variance and contrast
#' analysis. Psychological methods, 9(2), 164-82. doi:10.1037/1082-989X.9.2.164
#' @param posthocLetters Whether to also compute and show the letters
#' signifying differences between groups when conducting post hoc tests. This
#' requires package \code{multcompView} to be installed.
#' @param posthocLetterAlpha The alpha to use when determining whether groups
#' have different means when using \code{posthocLetters}.
#' @param silent Whether to show warnings and other diagnostic information or
#' remain silent.
#' @return A list of three elements: \item{input}{List with input arguments}
#' \item{intermediate}{List of intermediate objects, such as the aov and Anova
#' (from the car package) objects.} \item{output}{List with etasq, the effect
#' size, and dat, a dataframe with the Oneway Anova results.}
#' @note By my knowledge the Brown-Forsythe correction was not yet available in
#' R. I took this from the original paper (directed there by Field, 2014). Note
#' that this is the corrected \emph{F} value, not the Brown-Forsythe test for
#' normality!
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @references Brown, M., & Forsythe, A. (1974). \emph{The small sample
#' behavior of some statistics which test the equality of several means.}
#' Technometrics, 16(1), 129-132. https://doi.org/10.2307/1267501
#' 
#' Field, A. (2014) \emph{Discovering statistics using SPSS} (4th ed.). London:
#' Sage.
#' 
#' Steiger, J. H. (2004). \emph{Beyond the F test: Effect size confidence
#' intervals and tests of close fit in the analysis of variance and contrast
#' analysis}. Psychological methods, 9(2), 164-82.
#' doi:10.1037/1082-989X.9.2.164
#' @keywords utilities
#' @examples
#' 
#' ### Do a oneway Anova
#' oneway(y=ChickWeight$weight, x=ChickWeight$Diet);
#' 
#' ### Also order means and transpose the results
#' oneway(y=ChickWeight$weight, x=ChickWeight$Diet, means=TRUE, t=TRUE);
#' 
#' @export oneway
oneway <- function(y, x, posthoc=NULL, means=FALSE, fullDescribe=FALSE,
                   levene=FALSE, plot=FALSE, digits=2,
                   omegasq = TRUE,
                   etasq = TRUE,
                   corrections = FALSE,
                   pvalueDigits=3, t=FALSE, conf.level=.95,
                   posthocLetters = FALSE, posthocLetterAlpha = .05,
                   silent=FALSE) {
  
  res <- list(input = as.list(environment()));

  res$input$x.name <- extractVarName(deparse(substitute(x)));
  res$input$y.name <- extractVarName(deparse(substitute(y)));

  if (!is.numeric(y)) {
    stop("The y variable (", res$input$y.name, ") is not a numeric ",
        "vector! Note that in analysis of variance, the 'y' variable ",
        "must have at least the interval level of measurement!");
  }
  
  if (!is.factor(x)) {
    if (!silent) {
      warning("### Warning: the x variable (", res$input$x.name, ") is not a ",
             "factor! Converting it myself - but note that variables in R have ",
             "data types, and it's advisable to set these adequately (use for ",
             "example 'as.factor'; see '?as.factor' for help)!");
    }
    res$input$x.raw <- x;
    x <- as.factor(x);
    res$input$x <- x;
  }
  
  assign(res$input$x.name, x);
  assign(res$input$y.name, y);
  
  res$intermediate <- list();

  res$intermediate$aov <- aov(formula(paste0(res$input$y.name, " ~ ",
                                             res$input$x.name)));
  
  res$intermediate$Anova <- Anova(res$intermediate$aov, type=3);
  
  if (!is.null(posthoc)) {
    if (tolower(posthoc)=="tukey") {
      res$intermediate$posthoc <- TukeyHSD(res$intermediate$aov);
    }
    else if (tolower(posthoc)=="games-howell") {
      res$intermediate$posthocTGH <- posthocTGH(y=y, x=x, method="Games-Howell");
      res$intermediate$posthoc <- res$intermediate$posthocTGH$output$games.howell;
    }
    else {
      res$intermediate$posthoc <-
        pairwise.t.test(x=y, g=x, p.adjust.method=posthoc);
    }
  }
  if (means) {
    res$intermediate$means <- describeBy(y, x);
    tmpAttributes <- attributes(res$intermediate$means);
    res$intermediate$means <- lapply(res$intermediate$means, function(x) {
      class(x) <- 'data.frame';
      rownames(x)[1] <- ' ';
      return(x[, colnames(x) != 'vars']);
    });
    if (!fullDescribe) {
      res$intermediate$means <- lapply(res$intermediate$means, function(x) {
        return(x[, colnames(x) %in% c('n', 'mean', 'sd', 'se', 'median')]);
      });
    }
    if (t) {
      res$intermediate$means <- lapply(res$intermediate$means, t);
    }
    attributes(res$intermediate$means) <- tmpAttributes;
  }
  
  if (levene) {
    res$intermediate$leveneTest <- car::leveneTest(y, group=x, center=mean);
  }

  res$intermediate$etasq <- computeEffectSize_etasq(var1=x, var2=y,
                                                    conf.level=conf.level);
  
  res$intermediate$confIntOmegaSq <- confIntOmegaSq(var1=x, var2=y,
                                                    conf.level=conf.level);
  
  res$output <- list(etasq = res$intermediate$Anova$`Sum Sq`[2] /
                       sum(res$intermediate$Anova$`Sum Sq`[2:3]),
                     etasq.ci = res$intermediate$etasq$ci,
                     omegasq = res$intermediate$confIntOmegaSq$output$es,
                     omegasq.ci = res$intermediate$confIntOmegaSq$output$ci);
  
  res$output$dat <- data.frame(SS = res$intermediate$Anova$`Sum Sq`[2:3],
                               Df = res$intermediate$Anova$Df[2:3]);
  res$output$dat$MS <- res$output$dat$SS / res$output$dat$Df;
  res$output$dat[1, 'F'] <- res$intermediate$Anova$F[2];
  res$output$dat[1, 'p'] <- res$intermediate$Anova$`Pr(>F)`[2];
  row.names(res$output$dat) <- c('Between groups (error + effect)',
                                 'Within groups (error only)');
  
  if (corrections) {
    res$intermediate$welch <- oneway.test(formula(paste0(res$input$y.name,
                                                         " ~ ",
                                                         res$input$x.name)));
    
    ### Based on Brown & Forsythe (1974), found through Field (2014)
    SSm <- res$output$dat['Between groups (error + effect)', 'SS'];
    tmpDat <- na.omit(data.frame(x=x, y=y));
    groupVariances <- as.numeric(by(tmpDat$y, tmpDat$x, var));
    groupSizes <- as.numeric(by(tmpDat$y, tmpDat$x, length));
    denominator <- sum(groupVariances * (1 - ( groupSizes / sum(groupSizes))));
    res$intermediate$brown.forsythe <- list();
    res$intermediate$brown.forsythe$F <- SSm / denominator;

    res$intermediate$brown.forsythe$Df1 <- length(groupSizes) - 1;
    
    cValues <- ((1 - ( groupSizes / sum(groupSizes))) * groupVariances) /
      (sum( (1 - ( groupSizes / sum(groupSizes))) * groupVariances ));
    
    inverseDf2 <- sum(cValues^2 / (groupSizes - 1));
    
    res$intermediate$brown.forsythe$Df2 <- 1 / inverseDf2;
    
    res$intermediate$brown.forsythe$p <- pf(res$intermediate$brown.forsythe$F,
                                           res$intermediate$brown.forsythe$Df1,
                                           res$intermediate$brown.forsythe$Df2,
                                           lower.tail=FALSE);

  }
  
  if (plot) {
    res$intermediate$dat <- data.frame(x, y);
    names(res$intermediate$dat) <- c(res$input$x.name, res$input$y.name);
    res$output$plot <- dlvPlot(res$intermediate$dat,
                               x=res$input$x.name,
                               y=res$input$y.name)$plot +
      ggtitle(paste0(res$input$x.name, " and ",
                     res$input$y.name));
  }
  
  if (posthocLetters) {
    if (!requireNamespace("multcompView", quietly = TRUE)) {
      stop(paste0("You need to install the 'multcompView' package to obtain the ",
                  "letters illustrating which groups differ on post hoc tests."));
    } else {
      res$intermediate$posthocLetters.pValues <-
        res$intermediate$posthoc$p;
      res$intermediate$posthocLetters.logical <-
        res$intermediate$posthocLetters.pValues > posthocLetterAlpha;
      names(res$intermediate$posthocLetters.logical) <-
        row.names(res$intermediate$posthoc);
      res$output$posthocLetters <- 
        multcompView::multcompLetters(res$intermediate$posthocLetters.logical);
    }
  }
  
  class(res) <- 'oneway';
  return(res);
}

print.oneway <- function(x, digits=x$input$digits,
                         pvalueDigits=x$input$pvalueDigits,
                         na.print="", ...) {  
  
  if (x$input$plot) {
    print(x$output$plot);
  }
  
  cat(paste0("### Oneway Anova for y=", x$input$y.name,
             " and x=", x$input$x.name, " (groups: ",
             paste0(levels(x$input$x), collapse=", "),
             ")\n\n"));
  
  if (x$input$omegasq) {
    print(x$intermediate$confIntOmegaSq, digits=digits);
    cat('\n');
  }
  
  if (x$input$etasq) {
    cat(paste0("Eta Squared: ", round(x$input$conf.level * 100),
               "% CI = [", formatR(x$output$etasq.ci[1], digits=digits),
               "; ", formatR(x$output$etasq.ci[2], digits=digits),
               "], point estimate = ", formatR(x$output$etasq, digits=digits), "\n"));
  }

  if (x$input$omegasq | x$input$etasq) {
    cat('\n');
  }
  
  x$output$dat[, 1:4] <- round(x$output$dat[, 1:4], digits);

  ### Format p-values nicely
  x$output$dat$p <- formatPvalue(x$output$dat$p,
                                 digits=pvalueDigits,
                                 includeP=FALSE);
  
  ### Temporarily store row names and transform everything to character
  tmpRowNames <- row.names(x$output$dat);
  x$output$dat <- data.frame(lapply(x$output$dat, as.character));
  row.names(x$output$dat) <- tmpRowNames;

  if(x$input$t) {
    print(t(x$output$dat), na.print=na.print, quote=FALSE);
  } else {
    print(x$output$dat, na.print=na.print, quote=FALSE);
  }

  cat("\n");
  
  if (x$input$means) {
    cat(paste0("### Means for y (", x$input$y.name, ") separate for each level of x (", x$input$x.name, "):\n"));
    lapply(1:length(x$intermediate$means), function(index) {
      cat0("\n", x$input$x.name, " = ", names(x$intermediate$means[index]), ":\n");
      print(x$intermediate$means[[index]], digits=digits);
    });
  }

  if (x$input$levene) {
    cat0("\n### Levene's test for homogeneity of variance:\n\n",
         "F[", x$intermediate$leveneTest[1, 1],
         ", ", x$intermediate$leveneTest[2, 1],
         "] = ", round(x$intermediate$leveneTest[1, 2], digits),
         ", ", formatPvalue(x$intermediate$leveneTest[1, 3], digits=digits+1),
         ".\n");
  }
  
  if (!is.null(x$input$posthoc)) {
    cat0("\n### Post hoc test: ", x$input$posthoc,"\n\n");
    if (x$input$posthoc %IN% c('games-howell')) {
      x$intermediate$posthoc <- as.data.frame(x$intermediate$posthoc);
      x$intermediate$posthoc[, 1:(ncol(x$intermediate$posthoc)-1)] <-
        round(x$intermediate$posthoc[, 1:(ncol(x$intermediate$posthoc)-1)], digits);
      x$intermediate$posthoc[, ncol(x$intermediate$posthoc)] <-
        formatPvalue(x$intermediate$posthoc[, ncol(x$intermediate$posthoc)], digits=digits+1, includeP=FALSE);
      print(x$intermediate$posthoc, quote=FALSE);
    }
    else if (x$input$posthoc %IN% c('tukey')) {
      x$intermediate$posthoc <- lapply(x$intermediate$posthoc, function(x) {
        x[, 1:(ncol(x)-1)] <- round(x[, 1:(ncol(x)-1)], digits);
        x[, ncol(x)] <- formatPvalue(x[,ncol(x)], digits=digits+1, includeP=FALSE);
        return(x);
      });
      print(x$intermediate$posthoc[[1]], quote=FALSE);
    }
    else {
      x$intermediate$posthoc$p.value <- formatPvalue(x$intermediate$posthoc$p.value, digits=pvalueDigits, includeP=FALSE);
      print(x$intermediate$posthoc$p.value, quote=FALSE, na.print="");
    }
    
    if (!is.null(x$output$posthocLetters)) {
      cat("\n\n");
      print(x$output$posthocLetters);
    }
  }
  
  if (x$input$corrections) {
    cat0("\n### Welch correction for nonhomogeneous variances:\n\n",
         "F[", x$intermediate$welch$parameter[1],
         ", ", round(x$intermediate$welch$parameter[2], digits),
         "] = ", round(x$intermediate$welch$statistic, digits),
         ", ", formatPvalue(x$intermediate$welch$p.value, digits=digits+1),
         ".\n");
    cat0("\n### Brown-Forsythe correction for nonhomogeneous variances:\n\n",
         "F[", x$intermediate$brown.forsythe$Df1,
         ", ", round(x$intermediate$brown.forsythe$Df2, digits),
         "] = ", round(x$intermediate$brown.forsythe$F, digits),
         ", ", formatPvalue(x$intermediate$brown.forsythe$p, digits=digits+1),
         ".\n");    
  }

}


pander.oneway <- function(x, digits=x$input$digits,
                          pvalueDigits=x$input$pvalueDigits,
                          headerStyle = "**",
                          na.print="", ...) {  

  cat0("\n\n", headerStyle, "Oneway Anova for y=", x$input$y.name,
       " and x=", x$input$x.name, " (groups: ",
       vecTxt(levels(x$input$x)),
       ")", headerStyle, "\n\n");
  
  if (x$input$omegasq) {
    print(x$intermediate$confIntOmegaSq, digits=digits);
    cat('  \n');
  }
  
  if (x$input$etasq) {
    cat0("Eta Squared: ", round(x$input$conf.level * 100),
         "% CI = [", formatR(x$output$etasq.ci[1], digits=digits),
         "; ", formatR(x$output$etasq.ci[2], digits=digits),
         "], point estimate = ", formatR(x$output$etasq, digits=digits));
  }
  
  if (x$input$omegasq | x$input$etasq) {
    cat('\n\n');
  }
  
  x$output$dat[, 1:4] <- round(x$output$dat[, 1:4], digits);
  
  ### Format p-values nicely
  x$output$dat$p <- formatPvalue(x$output$dat$p,
                                 digits=pvalueDigits,
                                 includeP=FALSE);
  
  ### Temporarily store row names and transform everything to character
  tmpRowNames <- row.names(x$output$dat);
  x$output$dat <- data.frame(lapply(x$output$dat, as.character));
  row.names(x$output$dat) <- tmpRowNames;
  
  if(x$input$t) {
    pander(t(x$output$dat), missing="");
  } else {
    pander(x$output$dat, missing="");
  }
  
  if (x$input$plot) {
    grid.draw(x$output$plot);
  }
  
  if (x$input$means) {
    cat0("\n\n", headerStyle, "Means for y (", x$input$y.name,
         ") separate for each level of x (", x$input$x.name, "):",
         headerStyle);
    lapply(1:length(x$intermediate$means), function(index) {
      cat0("\n\n", x$input$x.name, " = ",
           names(x$intermediate$means[index]), ":  \n\n");
      pander(x$intermediate$means[[index]], digits=digits);
    });
    cat("\n");
  }
  
  if (x$input$levene) {
    cat0("\n\n", headerStyle, "Levene's test for homogeneity of variance:",
         headerStyle, "\n\n",
         "F<sub>", x$intermediate$leveneTest[1, 1],
         ", ", x$intermediate$leveneTest[2, 1],
         "</sub> = ", round(x$intermediate$leveneTest[1, 2], digits),
         ", ", formatPvalue(x$intermediate$leveneTest[1, 3], digits=digits+1),
         ".\n");
  }
  
  if (!is.null(x$input$posthoc)) {
    cat(paste0("\n\n", headerStyle, "Post hoc test: ", x$input$posthoc,
               headerStyle, "\n\n"));
    if (x$input$posthoc %IN% c('games-howell')) {
      x$intermediate$posthoc <- as.data.frame(x$intermediate$posthoc);
      x$intermediate$posthoc[, 1:(ncol(x$intermediate$posthoc)-1)] <-
        round(x$intermediate$posthoc[, 1:(ncol(x$intermediate$posthoc)-1)], digits);
      x$intermediate$posthoc[, ncol(x$intermediate$posthoc)] <-
        formatPvalue(x$intermediate$posthoc[, ncol(x$intermediate$posthoc)], digits=digits+1, includeP=FALSE);
      pander(x$intermediate$posthoc, missing="");
    }
    else if (x$input$posthoc %IN% c('tukey')) {
      x$intermediate$posthoc <- lapply(x$intermediate$posthoc, function(x) {
        x[, 1:(ncol(x)-1)] <- round(x[, 1:(ncol(x)-1)], digits);
        x[, ncol(x)] <- formatPvalue(x[,ncol(x)], digits=digits+1, includeP=FALSE);
        return(x);
      });
      pander(x$intermediate$posthoc[[1]], missing="");
    }
    else {
      x$intermediate$posthoc$p.value <- formatPvalue(x$intermediate$posthoc$p.value, digits=pvalueDigits, includeP=FALSE);
      pander(x$intermediate$posthoc$p.value, missing="");
    }
    
    if (!is.null(x$output$posthocLetters)) {
      cat("\n\n");
      pander(x$output$posthocLetters$Letters);
    }
  }

  if (x$input$corrections) {
    cat0("\n\n", headerStyle, "Welch correction for nonhomogeneous variances:",
         headerStyle, "\n\n",
         "F<sub>", x$intermediate$welch$parameter[1],
         ", ", round(x$intermediate$welch$parameter[2], digits),
         "</sub> = ", round(x$intermediate$welch$statistic, digits),
         ", ", formatPvalue(x$intermediate$welch$p.value, digits=digits+1),
         ".");
    cat0("\n\n", headerStyle, "Brown-Forsythe correction for nonhomogeneous variances:",
         headerStyle, "\n\n",
         "F<sub>", x$intermediate$brown.forsythe$Df1,
         ", ", round(x$intermediate$brown.forsythe$Df2, digits),
         "</sub> = ", round(x$intermediate$brown.forsythe$F, digits),
         ", ", formatPvalue(x$intermediate$brown.forsythe$p, digits=digits+1),
         ".\n");    
  }
  
  cat("\n\n");
  
}
