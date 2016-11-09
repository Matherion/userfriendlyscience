\name{detectRareWords}
\alias{detectRareWords}
\alias{textToWords}
\title{
Looking up word frequencies
}
\description{
This function checks, for each word in a text, how frequently it occurs in a given language. This is useful for eliminating rare words to make a text more accessible to an audience with limited vocabulary. \code{\link{htmlParse}} and \code{\link{xpathSApply}} from the \code{XML} package are used to process HTML files, if necessary. \code{textToWords} is a helper function that simply breaks down a character vector to a vector of words.
}
\usage{
detectRareWords(textFile = NULL,
                wordFrequencyFile = "Dutch",
                output = c("file", "show", "return"),
                outputFile = NULL,
                wordCol = "Word", freqCol = "FREQlemma",
                textToWordsFunction = "textToWords",
                encoding = "ASCII",
                xPathSelector = "/text()",
                silent = FALSE)
textToWords(characterVector)
}
\arguments{
  \item{textFile}{
  If NULL, a dialog will be shown that enables users to select a file. If not NULL, this has to be either a filename or a character vector. An HTML file can be provided; this will be parsed using 
}
  \item{wordFrequencyFile}{
  The file with word frequencies to use. If 'Dutch' or 'Polish', files from the Center for Reading Research (\url{http://crr.ugent.be/}) are downloaded.
}
  \item{output}{
  How to provide the output, as a character vector. If \code{file}, the filename to write to should be provided in \code{outputFile}. If \code{show}, the output is shown; and if \code{return}, the output is returned invisibly.
}
  \item{outputFile}{
  The name of the file to store the output in.
}
  \item{wordCol}{
  The name of the column in the \code{wordFrequencyFile} that contains the words.
}
  \item{freqCol}{
  The name of the column in the \code{wordFrequencyFile} that contains the frequency with which each word occurs.
}
  \item{textToWordsFunction}{
  The function to use to split a character vector, where each element contains one or more words, into a vector where each element is a word.
}
  \item{encoding}{
  The encoding used to read and write files.
}
  \item{xPathSelector}{
  If the file provided is an HTML file, \code{\link{xpathSApply}} is used to extract the content. \code{xPathSelector} specifies which content to extract (the default value extracts all text content).
}
  \item{silent}{
  Whether to suppress detailed feedback about the process.
}
  \item{characterVector}{
  A character vector, the elements of which are to be broken down into words.
}
}
\value{
\code{detectRareWords} return a dataframe (invisibly) if \code{output} contains \code{return}. Otherwise, NULL is returned (invisibly), but the output is printed and/or written to a file depending on the value of \code{output}.

\code{textToWords} returns a vector of words.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
\dontrun{
detectRareWords(paste('Dit is een tekst om de',
                      'werking van de detectRareWords',
                      'functie te demonstreren.'),
                output='show');
}
}
\keyword{ utils }
