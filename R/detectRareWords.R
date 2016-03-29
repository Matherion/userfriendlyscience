### Keuleers, E., Brysbaert, M. & New, B. (2010). SUBTLEX-NL: A new frequency
###   measure for Dutch words based on film subtitles. Behavior Research Methods,
###   42(3), 643-650.

detectRareWords <- function(textFile = NULL, wordFrequencyFile = "Dutch",
                            output = c('file', 'show', 'return'),
                            outputFile = NULL,
                            wordCol = 'Word',
                            FreqCol = 'FREQlemma',
                            textToWordsFunction = 'textToWords',
                            encoding="ASCII",
                            xPathSelector = '/text()',
                            silent=FALSE,
                            ...) {
  
  if (wordFrequencyFile %IN% c('Dutch', 'Polish')) {
    if (tolower(wordFrequencyFile) == 'dutch') {
      wfURL <- 'http://crr.ugent.be/subtlex-nl/SUBTLEX-NL.cd-above2.Rdata';
    } else if (tolower(wordFrequencyFile) == 'polish') {
      wfURL <- 'http://crr.ugent.be/subtlex-pl/subtlex-pl.RData'
    }
    con <- url(wfURL);
    wf <- load(con);
    wf <- get(wf);
    close(con);
  } else {
    wf <- getData(wordFrequencyFile);
  }
  
  if (is.null(textFile)) {
    ### If no filename is specified, request one from the user
    cat("You did not specify a file to open. Therefore, please select the",
        "file to open in the File Selection Dialog.",
        "Note that this dialog can sometimes appear behind the R window.",
        "If you do not see the file dialog now, use ALT-TAB or check the ",
        "start bar (in Windows), use COMMAND-TAB (in OSX), or check the ",
        "dock (in *nux based systems such as",
        "Ubuntu or OS X).");
    textFile <- file.choose();
  }
  
  textFileName <- textFile;
  
  extension <- tolower(gsub(".*\\.(.*)$", '\\1', textFile));
  
  if (extension == 'html') {
    ### Parse HTML to locate text content
    xData <- htmlParse(textFile, encoding=encoding);
    textFile <- xpathSApply(xData, xPathSelector, xmlValue);;
    
  } else {
    ### In other cases, assume .txt file

    ### Use separate connection to make sure proper encoding is selected
    con <- file(textFile, encoding=encoding);
    textFile <- readLines(con);
    close(con);
  }
  
  ### Get & store function to convert text to words
  textToWordsFn <- match.fun(textToWordsFunction);
  
  ### Remove punctuation, split into words, and remove duplicates
  words <- unique(tolower(textToWordsFn(textFile)));
  
  frequencies <- wf[match(words, tolower(wf[, wordCol])), FreqCol];
  
  dat <- data.frame(words, frequencies);
  
  dat <- dat[order(dat$frequencies, decreasing=TRUE), ];
  
  ### Now generate a plot with a line for the frequencies
  ### and a line for the used words. Use the density function to
  ### equalize the heights.
  
  if ('file' %IN% output) {
    
    if (is.null(outputFile)) {
      outputFile <- tolower(gsub("(.*)\\..*$", '\\1 [text analysis].txt', textFileName));
    }
    
    print(outputFile);
  
    outputExtension <- tolower(gsub(".*\\.(.*)$", '\\1', outputFile));
    if (outputExtension == 'csv') {
      write.csv(dat, outputFile);
    } else {
      writeLines(capture.output(print(dat, row.names=FALSE)), outputFile);
    }
    
    if (!silent) {
      cat0("\nFrequencies added for ", nrow(dat),
           " words; sorted list written to ", outputFile, ".\n");
    }
    
  }
  
  if ('show' %IN% output) {
    cat0("\nOutput here\n");
  }
  
  if ('return' %IN% output) {
    invisible(dat);
  }

}

textToWords <- function(characterVector,
                        language = 'Dutch') {
  
  ### Remove punctuation marks
  characterVector <- gsub('[[:punct:]]', '', characterVector);

  wordVector <- unlist(strsplit(characterVector, "[[:space:]]"));
  
  wordVector <- wordVector[wordVector != ''];
  
  ### Split into words and return
  return(wordVector);
  
}

# unPunctuate <- function(characterVector) {
#   return(gsub('[[:punct:]]', '', characterVector));
# }
