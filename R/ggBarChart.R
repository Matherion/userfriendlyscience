ggBarChart <- function(vector, plotTheme = theme_bw(), ...) {
  varName <- extractVarName(deparse(substitute(vector)));
  tmpDf <- as.data.frame(vector);
  names(tmpDf) <- varName;
  ggplot(tmpDf, aes_string(x=varName)) +
    geom_bar(...) + plotTheme;
}
