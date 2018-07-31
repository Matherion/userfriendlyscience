### Keuleers, E., Brysbaert, M. & New, B. (2010). SUBTLEX-NL: A new frequency
###   measure for Dutch words based on film subtitles. Behavior Research Methods,
###   42(3), 643-650.



#' Looking up word frequencies
#' 
#' This function checks, for each word in a text, how frequently it occurs in a
#' given language. This is useful for eliminating rare words to make a text
#' more accessible to an audience with limited vocabulary.
#' \code{\link{htmlParse}} and \code{\link{xpathSApply}} from the \code{XML}
#' package are used to process HTML files, if necessary. \code{textToWords} is
#' a helper function that simply breaks down a character vector to a vector of
#' words.
#' 
#' 
#' @aliases detectRareWords textToWords
#' @param textFile If NULL, a dialog will be shown that enables users to select
#' a file. If not NULL, this has to be either a filename or a character vector.
#' An HTML file can be provided; this will be parsed using
#' @param wordFrequencyFile The file with word frequencies to use. If 'Dutch'
#' or 'Polish', files from the Center for Reading Research
#' (\url{http://crr.ugent.be/}) are downloaded.
#' @param output How to provide the output, as a character vector. If
#' \code{file}, the filename to write to should be provided in
#' \code{outputFile}. If \code{show}, the output is shown; and if
#' \code{return}, the output is returned invisibly.
#' @param outputFile The name of the file to store the output in.
#' @param wordCol The name of the column in the \code{wordFrequencyFile} that
#' contains the words.
#' @param freqCol The name of the column in the \code{wordFrequencyFile} that
#' contains the frequency with which each word occurs.
#' @param textToWordsFunction The function to use to split a character vector,
#' where each element contains one or more words, into a vector where each
#' element is a word.
#' @param encoding The encoding used to read and write files.
#' @param xPathSelector If the file provided is an HTML file,
#' \code{\link{xpathSApply}} is used to extract the content.
#' \code{xPathSelector} specifies which content to extract (the default value
#' extracts all text content).
#' @param silent Whether to suppress detailed feedback about the process.
#' @param characterVector A character vector, the elements of which are to be
#' broken down into words.
#' @return \code{detectRareWords} return a dataframe (invisibly) if
#' \code{output} contains \code{return}. Otherwise, NULL is returned
#' (invisibly), but the output is printed and/or written to a file depending on
#' the value of \code{output}.
#' 
#' \code{textToWords} returns a vector of words.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utils
#' @examples
#' 
#' \dontrun{
#' detectRareWords(paste('Dit is een tekst om de',
#'                       'werking van de detectRareWords',
#'                       'functie te demonstreren.'),
#'                 output='show');
#' }
#' 
#' @export detectRareWords
detectRareWords <- function(textFile = NULL, wordFrequencyFile = "Dutch",
                            output = c('file', 'show', 'return'),
                            outputFile = NULL,
                            wordCol = 'Word',
                            freqCol = 'FREQlemma',
                            textToWordsFunction = 'textToWords',
                            encoding="ASCII",
                            xPathSelector = '/text()',
                            silent=FALSE) {
  
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
  
  if (file.exists(textFile)) {
    
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
    
  } else {
    if (!is.character(textFile)) {
      stop("In argument 'textFile', specify either a filename ",
           "containing the text to process, or a character vector.");
    }
  }
  ### Get & store function to convert text to words
  textToWordsFn <- match.fun(textToWordsFunction);
  
  ### Remove punctuation, split into words, and remove duplicates
  words <- unique(tolower(textToWordsFn(textFile)));
  
  frequencies <- wf[match(words, tolower(wf[, wordCol])), freqCol];
  
  dat <- data.frame(words, frequencies);
  
  dat <- dat[order(dat$frequencies, decreasing=TRUE), ];
  
  ### Now generate a plot with a line for the frequencies
  ### and a line for the used words. Use the density function to
  ### equalize the heights.
  
  if ('file' %IN% output) {
    
    if (is.null(outputFile)) {
      outputFile <- tolower(gsub("(.*)\\..*$", '\\1 [text analysis].txt', textFileName));
    }

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
    cat("\n");
    print(dat);
    cat("\n");
  }
  
  if ('return' %IN% output) {
    invisible(dat);
  } else {
    invisible(NULL);
  }

}

textToWords <- function(characterVector) {
  
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
