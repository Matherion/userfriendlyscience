###########################################################
###########################################################
###
### Collection of very basic functions
###
### File created by Gjalt-Jorn Peters. Questions? You can
### contact me through http://behaviorchange.eu.
###
###########################################################
###########################################################

### This function checks whether a package is installed;
### if not, it installs it. It then loads the package.
safeRequire <- function(packageName, mirrorIndex=NULL) {
  if (!is.element(packageName, installed.packages()[,1])) {
    if (!is.null(mirrorIndex)) {
      chooseCRANmirror(ind=mirrorIndex);
    }
    install.packages(packageName, dependencies=TRUE);
  }
  suppressPackageStartupMessages(require(package = packageName,
                                         character.only=TRUE,
                                         quietly=TRUE));
}

### trim simply trims spaces from the start and end of a string
trim <- function(str) {
  ### Based on 'trim' in package Gdata by
  ### Gregory R. Warnes <greg at warnes.net> and others
  str <- sub(pattern="^ +", replacement="", x=str)
  str <- sub(pattern=" +$", replacement="", x=str)
  str <- sub(pattern="^\t+", replacement="", x=str)
  str <- sub(pattern="\t+$", replacement="", x=str)
  return(str);
}

### Function to remove zero at start of number
noZero <- function (str) {
  return(gsub("0\\.", ".", str));  
}

### Function to format p values nicely
formatPvalue <- function (values, digits = 3, spaces=TRUE, includeP = TRUE) {
  missingValues <- is.na(values);
  values <- ifelse(values < 0, 0, ifelse(values > 1, 1, values));
  pchar <- ifelse(includeP, "p = ", "");
  eps <- 10 ^ -digits;
  res <- paste0(pchar, noZero(format.pval(round(values, digits),
                                          eps=eps, digits=digits,
                                          scientific=digits+1)));
  if (spaces) {
    res <- gsub("= <", "< ", res);
  } else {
    res <- gsub("= <", "<", res);
    res <- gsub(" ", "", res);
  }
  res <- ifelse(missingValues, NA, res);
  return(res);
}

### Function to format Pearson r
formatR <- function (r, digits) {
  return(noZero(round(r, digits)));
}

### repeat a string a given number of times
repeatStr <- function (str = " ", n = 1) {
  if (n < 1) {
    return("");
  }
  else if (n == 1) {
    return(str);
  }
  else {
    res <- str;
    for(i in c(1:(n-1))) {
      res <- paste0(res, str);
    }
    return(res);
  }
}

### The regular ifelse cannot return objects
ifelseObj <- function(condition, ifTrue, ifFalse) {
  if (condition) {
    return(ifTrue);
  }
  else {
    return(ifFalse);
  }
}

invertItems <- function(dat, items = NULL, ...) {
  if (is.null(items)) {
    items <- names(dat);
  } else if ((!is.character(items)) && (!is.numeric(items))) {
    stop("Argument 'items' is not a character vector or numeric vector ",
         "(but instead of type ", typeof(dat), ").");
  }
  usedDat <- dat[, items];
  
  ### Previous inversions
  prevInv <- lapply(dat[, items], attr, 'inverted');
  ### Replace NULL with FALSE
  prevInv <- lapply(prevInv, function(x) ifelse(is.null(x), FALSE, x));
  ### Warn if one or more items were already inverted
  if (sum(unlist(prevInv)) > 0) {
    alreadyInverted <- names(prevInv)[unlist(prevInv)];
    warning("Variables (columns) ", vecTxt(alreadyInverted, useQuote='"'),
            " have already been inverted! ",
            "Set ignorePreviousInversion to TRUE to override this check ",
            "and invert the vector anyway.");
    usedDat <- usedDat[, !(names(usedDat) %in% alreadyInverted)];
  }
  
  ### All convert factors to numeric vectors
  usedDat <- massConvertToNumeric(usedDat);
  
  ### Check whether any non-numeric vectors remain
  invalidVectors <- lapply(usedDat, is.numeric);
  if (FALSE %in% unlist(invalidVectors)) {
    invalidVectors <- names(invalidVectors)[!unlist(invalidVectors)];
    warning("Variables (columsn) ", vecTxt(invalidVectors, useQuote='"'),
            " have a type other than numeric or factor! Ignoring these.");
            usedDat <- usedDat[, !(names(usedDat) %in% invalidVectors)];
  }
  
  items <- names(usedDat);
  
  dat[, items] <- data.frame(lapply(usedDat, invertItem), ...);
  return(dat);
}

### To invert mirrored items
invertItem <- function(item, range=NULL, ignorePreviousInversion = FALSE) {
  ### Check whether this was already inverted
  if (!is.null(attr(item, "inverted"))) {
    if ((attr(item, "inverted") == TRUE) & !(ignorePreviousInversion)) {
      warning("Vector '", substitute(deparse(item)),
              "' has already been inverted! ",
              "Set ignorePreviousInversion to TRUE to override this ",
              "check and invert the vector anyway.");
    }
  }
  
  ### Not inverted yet (or ignorePreviousInversion set to TRUE)
  if (is.numeric(item)) {
    if (is.null(range)) {
      res <- sum(range(na.omit(item))) - item;
    }
    else {
      res <- sum(range(range)) - item;
    }
  }
  else {
    stop("Provide a numeric vector!");
  }
  attr(res, "inverted") <- TRUE;
  return(res);
}

### Basically what Marc Schwartz suggested at Thu Jul 1 19:10:28 CEST 2010
### on the R-help mailing list, see https://stat.ethz.ch/pipermail/r-help/2010-July/244299.html
is.odd <- function(vector) {
  return((vector %% 2) != 0);
}
is.even <- function(vector) {
  return((vector %% 2) == 0);
}

### Convert a vector to numeric values and trying to be smart about it.
convertToNumeric <- function (vector, byFactorLabel = FALSE) {
  ### Check whether the vector is datetime
  if (sum(sapply(class(vector), grepl, pattern='POSIX')) > 0) {
    return(vector);
  }
  if (!(is.factor(vector) | is.numeric(vector) |
          is.character(vector) | is.logical(vector))) {
    stop("Argument 'vector' must be a vector! Current class = '",
         class(vector), "'. To mass convert e.g. a dataframe, ",
         "use massConvertToNumber.");
  }
  if(is.factor(vector) && byFactorLabel) {
    ### Decimal symbol might be a comma instead of a period: convert
    ### factor to character vector and replace commas with periods
    vector <- as.numeric(gsub(as.character(vector), pattern=",", replacement="."));
    return();
  }
  else if (is.character(vector)) {
    return(suppressWarnings(as.numeric(gsub(as.character(vector), pattern=",", replacement="."))));
  }
  else {
    ### Thus, for numeric vectors; factors to be converted by index of the levels
    ### instead of by their labels; and logical vectors.
    return(as.numeric(vector));
  }
}

massConvertToNumeric <- function (dat, byFactorLabel = FALSE,
                                  ignoreCharacter = TRUE,
                                  stringsAsFactors = FALSE) {
  storedAttributes <- attributes(dat);
  dat <- data.frame(lapply(dat, function(x) {
    if (is.character(x) && ignoreCharacter) {
      return(x);
    }
    else {
      return(convertToNumeric(x, byFactorLabel = byFactorLabel));
    }
  }), stringsAsFactors=stringsAsFactors);
  attributes(dat) <- storedAttributes;
  return(dat);
}

vecTxt <- function(vector, delimiter = ", ", useQuote = "",
                   firstDelimiter = NULL, lastDelimiter = " & ",
                   firstElements = 0, lastElements = 1,
                   lastHasPrecedence = TRUE) {

  vector <- paste0(useQuote, vector, useQuote);
  
  if (length(vector) == 1) {
    return(vector);
  }
  
  if (firstElements + lastElements > length(vector)) {
    if (lastHasPrecedence) {
      firstElements <- length(vector) - lastElements;
    } else {
      lastElements <- length(vector) - firstElements;
    }
  }
  
  firstTxt <- lastTxt <- "";

  if (is.null(firstDelimiter)) {
    firstDelimiter <- delimiter;
  }
  if (is.null(lastDelimiter)) {
    lastDelimiter <- delimiter;
  }
  
  midBit <- vector;
  if (firstElements > 0) {
    firstBit <- head(vector, firstElements);
    midBit <- tail(vector, -firstElements);
    firstTxt <- paste0(paste0(firstBit, collapse=firstDelimiter), firstDelimiter);
  }
  if (lastElements > 0) {
    lastBit <- tail(vector, lastElements);
    midBit <- head(midBit, -lastElements);
    lastTxt <- paste0(lastDelimiter, paste0(lastBit, collapse=lastDelimiter));
  }
    
  midTxt <- paste0(midBit, collapse=delimiter);
  
  return(paste0(firstTxt, midTxt, lastTxt));
}

### Case insensitive '%in' variant
`%IN%` <- function(find, table) {
  return(toupper(find) %in% toupper(table));
}

### Paste0 but then immediately displaying on screen
cat0 <- function(..., sep="") {
  return(cat(..., sep=sep));
}


### Check whether elements are true, specifying how 'NA' should be seen
isTrue <- function(x, na = FALSE) {
  naValues <- ifelse(rep(na, length(x)),
                     is.na(x),
                     rep(FALSE, length(x)));
  return(ifelse(is.na(x), naValues, x==TRUE));
}

### Check whether something is a number
is.nr <- function(x) {
  if (!is.null(x)) {
    if (!is.na(x)) {
      if (is.numeric(x)) {
        return(TRUE);
      }
    }
  }
  return(FALSE);
}