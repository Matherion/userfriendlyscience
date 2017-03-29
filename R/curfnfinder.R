### Written by Nick Sabbe, http://stackoverflow.com/questions/7307987/logging-current-function-name

curfnfinder <- function(skipframes=0,
                        skipnames="(FUN)|(.+apply)|(replicate)",
                        retIfNone="Not in function",
                        retStack=FALSE,
                        extraPrefPerLevel="\t") {
  prefix <- sapply(3 + skipframes+1:sys.nframe(), function(i) {
    currv<-sys.call(sys.parent(n=i))[[1]]
    return(currv)
  });
  prefix[grep(skipnames, prefix)] <- NULL;
  prefix <- gsub("function \\(.*", "do.call", prefix);
  if(length(prefix)==0) {
    return(retIfNone);
  }
  else if(retStack) {
    return(paste(rev(prefix), collapse = "|"));
  }
  else {
    res <- as.character(unlist(prefix[1]));
    if (length(prefix) > 1) {
      res <- paste(paste(rep(extraPrefPerLevel, length(prefix) - 1), collapse=""), res, sep="");
    }
    return(res);
  }
}
