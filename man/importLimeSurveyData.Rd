\name{importLimeSurveyData}
\alias{importLimeSurveyData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
importLimeSurveyData
}
\description{
This function can be used to import files exported by LimeSurvey, a powerful
Open Source online survey application that can be used for, for example,
psychological experiments and other research.
}
\usage{
importLimeSurveyData(datafile = NULL,
  scriptfile = NULL,
  limeSurveyRegEx.varNames =
    "names\\\\(data\\\\)\\\\[\\\\d*\\\\] <- ",
  limeSurveyRegEx.toChar =
    "data\\\\[, \\\\d*\\\\] <- as.character\\\\(data\\\\[, \\\\d*\\\\]\\\\)",
  limeSurveyRegEx.varLabels =
    "attributes\\\\(data\\\\)\\\\$variable.labels\\\\[\\\\d*\\\\] <- \\".*\\"",
  limeSurveyRegEx.toFactor =
    paste0("data\\\\[, \\\\d*\\\\] <- factor\\\\(data\\\\[, \\\\d*\\\\], ",
           "levels=c\\\\(.*\\\\),labels=c\\\\(.*\\\\)\\\\)"),
  limeSurveyRegEx.varNameSanitizing =
    list(list(pattern = "#", replacement = "."),
         list(pattern = "\\\\$", replacement = ".")),
  setVarNames = TRUE,
  setLabels = TRUE,
  convertToCharacter = FALSE,
  convertToFactor = FALSE,
  categoricalQuestions = NULL,
  massConvertToNumeric = TRUE,
  dataHasVarNames = TRUE,
  encoding = "NULL",
  dataEncoding = "unknown",
  scriptEncoding = "ASCII")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datafile}{
  The path and filename of the file containing the data (comma separated values).
}
  \item{scriptfile}{
  The path and filename of the file containing the R script to import the data.
}
  \item{limeSurveyRegEx.varNames}{
  The regular expression used to extract the variable names from the script file. The
  first regex expression (i.e. the first expression between parentheses) will be
  extracted as variable name.
}
  \item{limeSurveyRegEx.toChar}{
  The regular expression to detect the lines in the import script where variables
  are converted to the character type.
}
  \item{limeSurveyRegEx.varLabels}{
  The regular expression used to detect the lines in the import script where
  variable labels are set.
}
  \item{limeSurveyRegEx.toFactor}{
  The regular expression used to detect the lines in the import script where
  vectors are converted to factors.
}
  \item{limeSurveyRegEx.varNameSanitizing}{
  A list of regular expression patterns and their replacements to sanitize the
  variable names (e.g. replace hashes/pound signs ('#') by something that is not
  considered the comment symbol by R).
}
  \item{setVarNames, setLabels, convertToCharacter, convertToFactor}{
  Whether to set variable names or labels, or convert to character or factor,
  using the code isolated using the specified regular expression.
}
  \item{categoricalQuestions}{
  Which variables (specified using LimeSurvey variable names)
  are considered categorical questions; for these, the script to convert
  the variables to factors, as extracted from the LimeSurvey import file, is
  applied.
}
  \item{massConvertToNumeric}{
  Whether to convert all variables to numeric using
  \code{\link{massConvertToNumeric}}.
}
  \item{dataHasVarNames}{
  Whether the variable names are included as header (first line) in the comma
  separated values file (data file).
}
  \item{encoding, dataEncoding, scriptEncoding}{
  The encoding of the files; \code{encoding} overrides \code{dataEncoding}
  and \code{scriptEncoding}, and so can be used to specify the same encoding
  for both.
}
}
\details{
This function was intended to make importing data from LimeSurvey a bit easier.
The default settings used by LimeSurvey are not always convenient, and this
function provides a bit more control.
}
\value{
The dataframe.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}

\seealso{
\code{\link{getData}}
}
\examples{
\dontrun{
### Of course, you need valid LimeSurvey files. This is an example of
### what you'd do if you have them, assuming you specified that path
### containing the data in 'dataPath', the name of the datafile in
### 'dataFileName', the name of the script file in 'dataLoadScriptName',
### and that you only want variables 'informedConsent', 'gender', 'hasJob',
### 'currentEducation', 'prevEducation', and 'country' to be converted to
### factors.
dat <- importLimeSurveyData(datafile = file.path(dataPath, dataFileName),
                            scriptfile = file.path(dataPath, dataLoadScriptName),
                            categoricalQuestions = c('informedConsent',
                                                     'gender',
                                                     'hasJob',
                                                     'currentEducation',
                                                     'prevEducation',
                                                     'country'));
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utility }
