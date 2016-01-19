posthocTGHci <- function(y, x, method=c("games-howell", "tukey"), conf.level = 0.95, digits=3) {
  ### Based on http://www.psych.yorku.ca/cribbie/6130/games_howell.R
  method <- tolower(method);
  tryCatch(method <- match.arg(method), error=function(err) {
    stop("Argument for 'method' not valid!");
  });
  
  res <- list(input = as.list(environment()));
  
  res$intermediate <- list(x = factor(x[complete.cases(x,y)]),
                           y = y[complete.cases(x,y)]);
  res$intermediate$n <- tapply(y, x, length);
  res$intermediate$groups <- length(res$intermediate$n);
  res$intermediate$df <- sum(res$intermediate$n) - res$intermediate$groups;
  res$intermediate$means <- tapply(y, x, mean);
  res$intermediate$variances <- tapply(y, x, var);
  res$intermediate$names <- levels(res$intermediate$x)
  res$intermediate$pairNames <- combn(res$intermediate$groups,2,function(ij){
    paste0(rev(res$intermediate$names[ij]),collapse="-");
  })
  
  res$intermediate$descriptives <- cbind(res$intermediate$n,
                                         res$intermediate$means,
                                         res$intermediate$variances);
  rownames(res$intermediate$descriptives) <- levels(res$intermediate$x);
  colnames(res$intermediate$descriptives) <- c('n', 'means', 'variances');
  
  ### Start on Tukey
  res$intermediate$errorVariance <-
    sum((res$intermediate$n-1) * res$intermediate$variances) /
    res$intermediate$df;
  res$intermediate$se <- combn(res$intermediate$groups,2, function(ij) {
    sqrt(res$intermediate$errorVariance*sum(1/res$intermediate$n[ij]));
  } )
  res$intermediate$dmeans <- combn(res$intermediate$groups, 2, function(ij) {
    diff(res$intermediate$means[ij]) } )
  res$intermediate$t <- abs(res$intermediate$dmeans)/res$intermediate$se
  res$intermediate$p.tukey <- ptukey(res$intermediate$t*sqrt(2),
                                     res$intermediate$groups,
                                     res$intermediate$df,
                                     lower.tail=FALSE);
  res$intermediate$alpha <- (1-conf.level)
  res$intermediate$qcrit <- qtukey(alpha,res$intermediate$groups,res$intermediate$df,lower.tail=FALSE)/sqrt(2)
  res$intermediate$tukey.low <- res$intermediate$dmeans - res$intermediate$qcrit*res$intermediate$se
  res$intermediate$tukey.high <- res$intermediate$dmeans + res$intermediate$qcrit*res$intermediate$se
  res$output <- list();
  res$output$tukey <- cbind(res$intermediate$dmeans,
                            res$intermediate$t,
                            res$intermediate$df,
                            res$intermediate$p.tukey,
                            res$intermediate$tukey.low,
                            res$intermediate$tukey.high)                           
  rownames(res$output$tukey) <- res$intermediate$pairNames;
  colnames(res$output$tukey) <- c('diff','t', 'df', 'p','ci.low','ci.high');
  
  ### Start on Games-Howell
  res$intermediate$df.corrected <- combn(res$intermediate$groups, 2, function(ij) {               
    sum(res$intermediate$variances[ij] /
          res$intermediate$n[ij])^2 / 
      sum((res$intermediate$variances[ij] /
             res$intermediate$n[ij])^2 / 
            (res$intermediate$n[ij]-1));
  } )
  res$intermediate$se.corrected <- combn(res$intermediate$groups,2, function(ij) {
    sqrt(sum(res$intermediate$variances[ij]/res$intermediate$n[ij]));
  } )
  res$intermediate$t.corrected <- abs(res$intermediate$dmeans)/res$intermediate$se.corrected
  res$intermediate$qcrit.corrected <- 
    qtukey(alpha,res$intermediate$groups,res$intermediate$df.corrected,lower.tail=FALSE)/sqrt(2)
  res$intermediate$gh.low <- res$intermediate$dmeans - 
    res$intermediate$qcrit.corrected*res$intermediate$se.corrected
  res$intermediate$gh.high <- res$intermediate$dmeans + 
    res$intermediate$qcrit.corrected*res$intermediate$se.corrected


  res$intermediate$p.gameshowell <- ptukey(res$intermediate$t.corrected*sqrt(2),
                                           res$intermediate$groups,
                                           res$intermediate$df.corrected,
                                           lower.tail=FALSE)  
  res$output$games.howell <- cbind(res$intermediate$dmeans,
                                   res$intermediate$t.corrected,
                                   res$intermediate$df.corrected,
                                   res$intermediate$p.gameshowell,
                                   res$intermediate$gh.low,
                                   res$intermediate$gh.high);
  rownames(res$output$games.howell) <- res$intermediate$pairNames;
  colnames(res$output$games.howell) <- c('diff','t', 'df', 'p','ci.low', 'ci.high');
  
  ### Set class and return object
  class(res) <- 'posthocTGH';
  return(res);
  
}

print.posthocTGH <- function(x, digits=x$input$digits, ...) {
  print(x$intermediate$descriptives, digits=digits);
  cat('\n');
  if (x$input$method == 'tukey') {
    print(x$output$tukey, digits=digits);
  }
  else if (x$input$method == 'games-howell') {
    print(x$output$games.howell, digits=digits);
  }
}
