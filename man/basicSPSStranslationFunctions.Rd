\name{basicSPSStranslationFunctions}
\alias{basicSPSStranslationFunctions}
\alias{getData}
\alias{getDat}
\alias{exportToSPSS}
\alias{filterBy}
\alias{useAll}
\alias{mediaan}
\alias{modus}
\title{
Basic SPSS translation functions
}
\description{
Basic functons to make working with R easier for SPSS users: getData and getDat provide an easy way to load SPSS datafiles, and exportToSPSS to write to a datafile and syntax file that SPSS can import; filterBy and useAll allow easy temporary filtering of rows from the dataframe; mediaan and modus compute the median and mode of ordinal or numeric data.
}
\usage{
getData(filename = NULL, file = NULL, errorMessage =
        "[defaultErrorMessage]", applyRioLabels = TRUE,
        use.value.labels = FALSE, to.data.frame = TRUE,
        stringsAsFactors = FALSE, silent=FALSE, ...)
        
getDat(..., dfName = "dat", backup = TRUE)

exportToSPSS(dat, savfile = NULL, datafile = NULL, codefile = NULL,
             fileEncoding = "UTF-8", newLinesInString = " |n| ")
             
filterBy(dat, expression, replaceOriginalDataframe = TRUE,
         envir = parent.frame())
         
useAll(dat, replaceFilteredDataframe = TRUE)

mediaan(vector)

modus(vector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename, file}{
  It is possible to specify a path and filename to load here. If not specified, the default R file selection dialogue is shown. \code{file} is still available for backward compatibility but will eventually be phased out.
}
  \item{errorMessage}{
  The error message that is shown if the file does not exist or does not have the right extension; "[defaultErrorMessage]" is replaced with
  a default error message (and can be included in longer messages).
}
  \item{applyRioLabels}{
  Whether to apply the labels supplied by Rio. This will make variables that has value labels into factors.
}
  \item{use.value.labels}{
  Only useful when reading from SPSS files: whether to read variables with value labels as factors (TRUE) or numeric vectors (FALSE).
}
  \item{to.data.frame}{
  Only useful when reading from SPSS files: whether to return a dataframe or not.
}
  \item{stringsAsFactors}{
  Whether to read strings as strings (FALSE) or factors (TRUE).
}
  \item{silent}{
  Whether to suppress potentially useful information.
}
  \item{...}{
  Additional options, passed on to the function used to import the data (which depends on the extension of the file).
}
  \item{dfName}{
  The name of the dataframe to create in the parent environment.
}
  \item{backup}{
  Whether to backup an object with name \code{dfName}, if one already exists
  in the parent environment.
}
\item{dat}{
  Dataframe to process: for filterBy, dataframe to filter rows from; for useAll, dataframe to restore ('unfilter').
}
\item{datafile}{
  The name of the data file, a comma separated values file that can be read into SPSS by using the code file.
}
\item{codefile}{
  The name of the code file, the SPSS syntax file that can be used to import the data file.
}
\item{savfile}{
  The name of the SPSS format .sav file (alternative for writing a datafile and a codefile).
}
\item{fileEncoding}{
  The encoding to use to write the files.
}
\item{newLinesInString}{
  A string to replace newlines with (SPSS has problems reading newlines).
}
\item{expression}{
  Logical expression determining which rows to keep and which to drop. Can be either a logical vector or a string which is then evaluated. If it's a string, it's evaluated using 'with' to evaluate the expression using the variable names.
}
  \item{replaceOriginalDataframe}{
  Whether to also replace the original dataframe in the parent environment. Very messy, but for maximum compatibility with the 'SPSS way of doing things', by default, this is true. After all, people who care about the messiness/inappropriateness of this function wouldn't be using it in the first place :-)
}
  \item{envir}{
  The environment where to create the 'backup' of the unfiltered dataframe, for when useAll is called and the filter is deactivated again.
}
  \item{replaceFilteredDataframe}{
  Whether to replace the filtered dataframe passed in the 'dat' argument (see replaceOriginalDataframe).
}
  \item{vector}{
  For mediaan and modus, the vector for which to find the median or mode.
  }
}
\value{

getData returns the imported dataframe, with the filename from which it was read stored in the 'filename' attribute.

getDat is a simple wrapper for \code{getData()} which creates a dataframe in
the parent environment, by default with the name 'dat'. Therefore, calling
\code{getDat()} in the console will allow the user to select a file, and
the data from the file will then be read and be available as 'dat'. If an
object with \code{dfName} (i.e. 'dat' by default) already exists, it will
be backed up with a warning. \code{getDat()} therefore returns nothing.

mediaan returns the median, or, in the case of a factor where the median is in between two categories, both categories.

modus returns the mode.

}
\note{
getData() currently can't read from LibreOffice or OpenOffice files. There doesn't seem to be a platform-independent package that allows this. Non-CRAN package ROpenOffice from OmegaHat should be able to do the trick, but fails to install (manual download and installation using http://www.omegahat.org produces "ERROR: dependency 'Rcompression' is not available for package 'ROpenOffice'" - and manual download and installation of RCompression produces "Please define LIB_ZLIB; ERROR: configuration failed for package 'Rcompression'"). If you have any suggestions, please let me know!
}
\examples{

### Open a dialogue to read an SPSS file
### (wrapped in 'if (interactive())' to prevent execution during
###  the testing of the examples when building the package)
if (interactive()) {
  getData();
}

### Get a median and a mode
mediaan(c(1,2,2,3,4,4,5,6,6,6,7));
modus(c(1,2,2,3,4,4,5,6,6,6,7));

### Create an example dataframe
(exampleDat <- data.frame(x=rep(8, 8), y=rep(c(0,1), each=4)));
### Filter it, replacing the original dataframe
(filterBy(exampleDat, "y=0"));
### Restore the old dataframe
(useAll(exampleDat));

}
\keyword{ utilities }
\keyword{ file }
\keyword{ univar }
