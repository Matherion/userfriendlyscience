### Function to remove values that compromise data integrity
checkDataIntegrity <- function(x, dat, newValue = NA,
                               removeCases = FALSE,
                               validValueSuffix = "_validValue",
                               newValueSuffix = "_newValue",
                               totalVarName = "numberOfInvalidValues",
                               append=TRUE,
                               replace=TRUE,
                               silent=FALSE,
                               rmarkdownOutput=FALSE,
                               callingSelf=FALSE) {
  dataIntegrityLog <- "";
  if (callingSelf && removeCases) {
    dataIntegrityLog <- addToLog(fullLog=dataIntegrityLog,
                                 showLog=!silent,
                                 paste0("Dataframe currently at ",
                                        nrow(dat), " rows.\n"));
  } else if (!callingSelf && removeCases) {
    dataIntegrityLog <- addToLog(fullLog=dataIntegrityLog,
                                 showLog=!silent,
                                 paste0("Processing dataframe '",
                                        deparse(substitute(dat)),
                                        "'.\n"));
  }
  if (is.list(x)) {
    ### If we're provided with a list, call ourselves again, but then
    ### separately for each vector (pair) in the list. First once, to
    ### create the dataframe.
    dat <- checkDataIntegrity(x[[1]],
                              dat,
                              newValue=newValue,
                              removeCases=removeCases,
                              validValueSuffix=validValueSuffix,
                              totalVarName=totalVarName,
                              append=TRUE,
                              replace=replace,
                              silent=silent,
                              callingSelf=TRUE);
    if (length(x) > 1) {
      ### Then, if necessary, more often.
      for (i in 2:length(x)) {
        dat <-
          checkDataIntegrity(x[[i]],
                             dat,
                             newValue=newValue,
                             removeCases=removeCases,
                             validValueSuffix=validValueSuffix,
                             totalVarName=totalVarName,
                             append=TRUE,
                             replace=replace,
                             silent=silent,
                             callingSelf=TRUE);
      }
    }
    if (removeCases) {
      dataIntegrityLog <- addToLog(fullLog=dataIntegrityLog,
                                   showLog=!silent,
                                   paste0("Returning dataframe with ",
                                          nrow(dat), " rows.\n"));
    }
    attr(dat, 'checkDataIntegrity_log') <-
      paste0(attr(dat, 'checkDataIntegrity_log'), dataIntegrityLog);
    ### Remove empty lines from log string
    attr(dat, 'checkDataIntegrity_log') <-
      gsub("\n\n", "\n", attr(dat, 'checkDataIntegrity_log'));
    return(dat);
  } else {
    if (length(x) != 2) {
      if (!silent) warning("Provide a vector of length 2; instead, a vector of length ", length(x),
                           " was provided!");
    }
    dataIntegrityLog <- addToLog(fullLog=dataIntegrityLog,
                                 showLog=!silent,
                                 paste0(ifelse(rmarkdownOutput, "* ", ""),
                                        "Matching cases to criterion '", x[2],
                                        "' for all variables matching regular expression '",
                                        ifelse(rmarkdownOutput, "`", ""),
                                        x[1],
                                        ifelse(rmarkdownOutput, "`", ""),
                                        "'.\n"));
    ### If we're not provided with a list, we're provided with a vector
    varNames <- grep(x[1], names(dat), value=TRUE);
    if (length(varNames) == 0) {
      dataIntegrityLog <- addToLog(fullLog=dataIntegrityLog,
                                   showLog=!silent,
                                   paste0("No variables in the dataframe match regular expression '",
                                          ifelse(rmarkdownOutput, "`", ""),
                                          x[1],
                                          ifelse(rmarkdownOutput, "`", ""),
                                          "'.\n"));
      totalInvalidValues <- rep(NA, nrow(dat));
    } else {
      validValueVectors <- as.data.frame(lapply(varNames,
                                                function(currentName) {
                                                  return(eval(parse(text=paste0('with(dat, ',
                                                                                currentName, x[2], ')'))));
                                                }));
      names(validValueVectors) <- paste0(varNames, validValueSuffix);
      affectedRows <- data.frame(varNames = varNames,
                                 nrOfRows = colSums(!validValueVectors, na.rm=TRUE));
      totalInvalidValues <- apply(!validValueVectors, 1, sum);
      allValid <- apply(validValueVectors, 1, all);
      if (removeCases) {
        if (!callingSelf) {
          dataIntegrityLog <- addToLog(fullLog=dataIntegrityLog,
                                       showLog=!silent,
                                       paste0("Processing dataframe '",
                                              deparse(substitute(dat)),
                                              "' with ",
                                              nrow(dat), " rows.\n"));
        }
        ### Don't bother setting new values, just delete the cases and return the result
        dataIntegrityLog <- addToLog(fullLog=dataIntegrityLog,
                                     showLog=!silent,
                                     paste0("Removed ", affectedRows$nrOfRows,
                                            " rows because they did not satisfy criterion '",
                                            affectedRows$varNames, x[2],
                                            "'.\n", collapse=""));
        dat <- dat[allValid, ];
        if (!callingSelf) {
          dataIntegrityLog <- addToLog(fullLog=dataIntegrityLog,
                                       showLog=!silent,
                                       paste0("Returning dataframe with ",
                                              nrow(dat), " rows.\n"));
        }
      } else {
        dataIntegrityLog <- addToLog(fullLog=dataIntegrityLog,
                                     showLog=!silent,
                                     paste0(ifelse(rmarkdownOutput, "    * ", ""),
                                            "In ", affectedRows$nrOfRows,
                                            " rows, for variable '",
                                            affectedRows$varNames,
                                            "', replacing values that do not satisfy criterion '",
                                            affectedRows$varNames, x[2],
                                            "' with '", newValue,
                                            "'.\n", collapse=""));
        ### Create a vector with the new values
        newValueVectors <- as.data.frame(lapply(seq_along(varNames),
                                                function(i) {
                                                  return(ifelse(validValueVectors[, i],
                                                                dat[, varNames[i]],
                                                                newValue));
                                                }));
        names(newValueVectors) <- paste0(varNames, newValueSuffix);
        if (!append) {
          ### Return whether values are valid or not plus the new values
          dat <- cbind(validValueVectors, newValueVectors);
        } else {
          ### Append them and then return them
          dat[, names(validValueVectors)] <- validValueVectors;
          if (replace) {
            dat[, varNames] <- newValueVectors;
          } else {
            dat[, names(newValueVectors)] <- newValueVectors;
          }
        }
      }
      if (!removeCases && !is.null(totalVarName)) {
        dat[, totalVarName] <- ifelseObj(totalVarName %in% names(dat),
                                         dat[, totalVarName] + totalInvalidValues,
                                         totalInvalidValues);
      }
    }
    ### Set attributes and return result
    if (exists('validValueVectors')) {
      attr(dat, 'checkDataIntegrity_validValueVectors') <-
        ifelseObj(is.null(attr(dat, 'checkDataIntegrity_validValueVectors')),
                  validValueVectors,
                  ifelseObj(is.data.frame(attr(dat, 'checkDataIntegrity_validValueVectors')),
                            list(attr(dat, 'checkDataIntegrity_validValueVectors'), validValueVectors),
                            c(attr(dat, 'checkDataIntegrity_validValueVectors'), list(validValueVectors))));
    }
    if (exists('allValid')) {
      attr(dat, 'checkDataIntegrity_retainedCases') <-
        ifelseObj(is.null(attr(dat, 'checkDataIntegrity_retainedCases')),
                  allValid,
                  ifelseObj(is.list(attr(dat, 'checkDataIntegrity_retainedCases')),
                            c(attr(dat, 'checkDataIntegrity_retainedCases'), list(allValid)),
                            c(list(attr(dat, 'checkDataIntegrity_retainedCases')), list(allValid))));
    }
    if (exists('affectedRows')) {
      attr(dat, 'checkDataIntegrity_affectedRows') <-
        ifelseObj(is.null(attr(dat, 'checkDataIntegrity_affectedRows')),
                  affectedRows,
                  ifelseObj(is.data.frame(attr(dat, 'checkDataIntegrity_affectedRows')),
                            list(attr(dat, 'checkDataIntegrity_affectedRows'), affectedRows),
                            c(attr(dat, 'checkDataIntegrity_affectedRows'), list(affectedRows))));
    }
    if (exists('totalInvalidValues')) {
      attr(dat, 'checkDataIntegrity_totalInvalidValues') <-
        ifelseObj(is.null(attr(dat, 'checkDataIntegrity_totalInvalidValues')),
                  totalInvalidValues,
                  ifelseObj(is.vector(attr(dat, 'checkDataIntegrity_totalInvalidValues')) &&
                              length(attr(dat, 'checkDataIntegrity_totalInvalidValues')) == length(totalInvalidValues),
                            attr(dat, 'checkDataIntegrity_totalInvalidValues') + totalInvalidValues,
                            ifelseObj(is.list(attr(dat, 'checkDataIntegrity_totalInvalidValues')),
                                      c(attr(dat, 'checkDataIntegrity_totalInvalidValues'), list(totalInvalidValues)),
                                      list(attr(dat, 'checkDataIntegrity_totalInvalidValues'), totalInvalidValues))));
    }
    attr(dat, 'checkDataIntegrity_log') <-
      paste0(attr(dat, 'checkDataIntegrity_log'), dataIntegrityLog);
    ### Remove empty lines from log string
    attr(dat, 'checkDataIntegrity_log') <-
      gsub("\n\n", "\n", attr(dat, 'checkDataIntegrity_log'));
    
    # if (rmarkdownOutput) {
    #   ### Add bullets
    #   attr(dat, 'checkDataIntegrity_log') <- gsub("Matching cases to criterion",
    #                                              "\n* Matching cases to criterion",
    #                                              attr(dat, 'checkDataIntegrity_log'),
    #                                              fixed=TRUE);
    #   
    #   attr(dat, 'checkDataIntegrity_log') <- gsub("In ([0-9]+) rows,",
    #                                               "\n    * In \\1 rows,",
    #                                               attr(dat, 'checkDataIntegrity_log'));
    #   
    #   ### Escape dollar signs
    #   attr(dat, 'checkDataIntegrity_log') <- gsub("regular expression '(.*)'.",
    #                                               "regular expression '`\\1`'.",
    #                                               attr(dat, 'checkDataIntegrity_log'));
    #   
    # }
    
    return(dat);
  }
}
