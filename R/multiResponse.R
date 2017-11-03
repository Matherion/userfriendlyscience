### For the analysis of multiple response questions, use this function:
### https://stackoverflow.com/questions/9265003/analysis-of-multiple-response
multiResponse = function(data, items=NULL, regex = NULL, endorsedOption = 1) {
  if (is.null(regex) && is.null(items)) {
    items <- names(data);
  } else if (is.null(items)) {
    items <- grep(regex, names(data), value=TRUE)
  }
  data = data[, items];
  nrOfEndorsements = sum(data == endorsedOption, na.rm=TRUE);
  endorsementsPerItem = colSums(data == endorsedOption, na.rm=TRUE);
  ### Number of participants; first look for missing values in each
  ### row, then only count participants with at least one valid
  ### value
  nrOfCases = sum(!apply(apply(data, 1, is.na), 2, all));
  totals = as.numeric(c(endorsementsPerItem, nrOfEndorsements));
  res <- data.frame(c(names(endorsementsPerItem), "Total"),
                    totals,
                    (totals/nrOfEndorsements)*100,
                    (totals/nrOfCases)*100);
  names(res) <- c("Option", "Frequency", "Percentage of responses", paste0("Percentage of (", nrOfCases, ") cases"));
  return(res);  
}
