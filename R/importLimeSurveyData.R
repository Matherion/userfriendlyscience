importLimeSurveyData <- function(datafile = NULL,
                                 dataPath = NULL,
                                 datafileRegEx = NULL,
                                 scriptfile = NULL,
                                 limeSurveyRegEx.varNames =
                                   "names\\(data\\)\\[\\d*\\] <- ",
                                 limeSurveyRegEx.toChar =
                                   "data\\[, \\d*\\] <- as.character\\(data\\[, \\d*\\]\\)",
                                 limeSurveyRegEx.varLabels =
                                   "attributes\\(data\\)\\$variable.labels\\[\\d*\\] <- \".*\"",
                                 limeSurveyRegEx.toFactor =
                                   paste0("data\\[, \\d*\\] <- factor\\(data\\[, \\d*\\], ",
                                          "levels=c\\(.*\\),labels=c\\(.*\\)\\)"),
                                 limeSurveyRegEx.varNameSanitizing =
                                   list(list(pattern = "#", replacement = "."),
                                        list(pattern = "\\$", replacement = ".")),
                                 setVarNames = TRUE,
                                 setLabels = TRUE,
                                 convertToCharacter = FALSE,
                                 convertToFactor = FALSE,
                                 categoricalQuestions = NULL,
                                 massConvertToNumeric = TRUE,
                                 dataHasVarNames = TRUE,
                                 encoding=NULL,
                                 dataEncoding='unknown', #'UTF-8',
                                 scriptEncoding='ASCII') {
  
  if (!is.null(encoding)) {
    dataEncoding <- scriptEncoding <- encoding;
  }

  ### Set filename(s) to read
  if (!is.null(dataPath) && !is.null(datafileRegEx)) {
    files <- unique(list.files(path = dataPath,
                               pattern = datafileRegEx,
                               ignore.case = TRUE,
                               recursive=TRUE,
                               full.names=TRUE));
    
  } else if (!is.null(datafile)) {
    if (!file.exists(datafile)) {
      stop("File specified as datafile ('", datafile, "') does not exist!");
    } else {
      files <- datafile;
    }
  } else {
    stop("Please specify a datafile to read, or a datafileRegEx to read multiple datafiles!");
  }
  
  ### Load datafile(s)
  data <- NULL;
  for (currentDatafile in files) {
    print(currentDatafile);
    if (dataHasVarNames) {
      currentData <- getData(currentDatafile, quote = "'\"", na.strings=c("", "\"\""),
                             stringsAsFactors=FALSE, encoding=dataEncoding, header=TRUE);
    } else {
      currentData <- getData(currentDatafile, quote = "'\"", na.strings=c("", "\"\""),
                             stringsAsFactors=FALSE, encoding=dataEncoding, header=FALSE);
    }
    if (is.null(data)) {
      data <- currentData;
    } else {
      data <- rbind(data, currentData);
    }
  }

  ### Load scriptfile
  if (!is.null(scriptfile)) {
    if (!file.exists(scriptfile)) {
      stop("File specified as scriptfile ('", scriptfile, "') does not exist!");
    }
    ### Use separate connection to make sure proper encoding is selected
    con <- file(scriptfile, encoding=scriptEncoding)
    datascript <- readLines(con);
    close(con);
    varNamesScript <- datascript[grepl(limeSurveyRegEx.varNames,
                                       datascript)];
    varLabelsScript <- datascript[grepl(limeSurveyRegEx.varLabels,
                                        datascript)];
    toCharScript <- datascript[grepl(limeSurveyRegEx.toChar,
                                     datascript)];
    toFactorScript <- datascript[grepl(limeSurveyRegEx.toFactor,
                                       datascript)];
    if (setVarNames) {
      eval(parse(text=varNamesScript));
    }
    if (setLabels) {
      eval(parse(text=varLabelsScript));
    }
    if (convertToCharacter) {
      eval(parse(text=toCharScript));
    }
    if (convertToFactor || (!is.null(categoricalQuestions))) {
      if (massConvertToNumeric) {
        data <- massConvertToNumeric(data);
      }
      if (!is.null(categoricalQuestions)) {
        if (setVarNames) {
          varNames <- names(data);
        } else {
          stop("You can't set setVarNames to FALSE and also set ",
               "categoricalQuestions to anything else than NULL, ",
               "because the content of categoricalQuestions should ",
               "be the LimeSurvey variables names!");
        }
        toFactorScript <- unlist(lapply(as.list(categoricalQuestions),
                                        function(x, string=toFactorScript,
                                                 varNms=varNames) {
                                          return(grep(paste0("data\\[, ",
                                                             which(varNms==x),
                                                             "\\] <-"),
                                                      string, value=TRUE));
                                        }));
      }
      eval(parse(text=toFactorScript));
    }
  } else {
    if (massConvertToNumeric) {
      data <- massConvertToNumeric(data);
    }
  }
  if (length(limeSurveyRegEx.varNameSanitizing)) {
    for (currentRegexPair in limeSurveyRegEx.varNameSanitizing) {
      names(data) <- gsub(currentRegexPair$pattern,
                          currentRegexPair$replacement,
                          names(data));
    }
  }

  return(data);
}
