### Written by Nick Sabbe, http://stackoverflow.com/questions/7307987/logging-current-function-name



#' Function to find the name of the calling function
#' 
#' This function finds and returns the name of the function calling it. This
#' can be useful, for example, when generating functions algorithmically.
#' 
#' This function was written by Nick Sabbe for his package \code{addendum}.  He
#' posted it on Stack Exchange at
#' \url{http://stackoverflow.com/questions/7307987/logging-current-function-name}
#' and I included this here with this permission.
#' 
#' @param skipframes Number of frames to skip; useful when called from an
#' anonymous function.
#' @param skipnames A regular expression specifying which substrings to delete.
#' @param retIfNone What to return when called from outside a function.
#' @param retStack Whether to return the entire stack or just one function.
#' @param extraPrefPerLevel Extra prefixes to return for each level of the
#' function.
#' @return The current function.
#' @author Nick Sabbe (Arteveldehogeschool)
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utility
#' @examples
#' 
#'   functionA <- functionB <- function() {
#'     curFn <- curfnfinder();
#'     if (curFn == 'functionA') {
#'       cat('Doing something\n');
#'     } else {
#'       cat('Doing something else\n');
#'     }
#'     cat('Doing something generic.');
#'   }
#'   functionA();
#'   functionB();
#' 
#' @export curfnfinder
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
