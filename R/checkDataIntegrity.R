### Function to remove values that compromise data integrity


#' Conveniently checking data integrity
#' 
#' This function is designed to make it easy to perform some data integrity
#' checks, specifically checking for values that are impossible or unrealistic.
#' These values can then be replaced by another value, or the offending cases
#' can be deleted from the dataframe.
#' 
#' 
#' @param x This can be either a vector or a list. If it is a vector, it should
#' have two elements, the first one being a regular expression matching one or
#' more variables in the dataframe specified in \code{dat}, and second one
#' being the condition the matching variables have to satisfy. If it is a list,
#' it should be a list of such vectors. The conditions should start with a
#' \code{\link{Comparison}} operator followed by a value (e.g. "<30" or ">=0).
#' @param dat The dataframe containing the variables of which we should check
#' the integrity.
#' @param newValue The new value to be assigned to cases not satisfying the
#' specified conditions.
#' @param removeCases Whether to delete cases that do not satisfy the criterion
#' from the dataframe (if \code{FALSE}, they're not deleted, but the offending
#' value is replaced by \code{newValue}).
#' @param validValueSuffix Suffix to append to variable names when creating
#' variable names for new variables that contain TRUE and FALSE to specify for
#' each original variable whether its value satisfied the specified criterion.
#' @param newValueSuffix If \code{replace} is \code{FALSE}, original values are
#' not replaced, but instead new variables are created where the offending
#' values have been replaced. This suffix is appended to each original variable
#' name to create the new variable name.
#' @param totalVarName This is the name of a variable that contains, for each
#' case, the total number of invalid values among all variables checked.
#' @param append Whether to append the columns to the dataframe, or only return
#' the new columns.
#' @param replace Whether to replace the offending values with the value
#' specified in \code{newValue} or whether to create new columns (see
#' \code{newValueSuffix}).
#' @param silent Whether to display the log, or only set it as attribute of the
#' returned dataframe.
#' @param rmarkdownOutput Whether to format the log so that it's ready to be
#' included in RMarkdown reports.
#' @param callingSelf For internal use; whether the function calls itself.
#' @return The dataframe with the corrections, and the log stored in attribute
#' \code{checkDataIntegrity_log}.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utilities
#' @examples
#' 
#' ### Default behavior: return dataframe with
#' ### offending values replaced by NA
#' 
#' checkDataIntegrity(c('mpg', '<30'),
#'                    mtcars);
#' 
#' ### Check two conditions, and instead of returning the
#' ### dataframe with the results appended, only return the
#' ### columns indicating which cases 'pass', what the new
#' ### values would be, and how many invalid values were
#' ### found for each case (to easily remove cases that
#' ### provided many invalid values)
#' 
#' checkDataIntegrity(list(c('mpg', '<30'),
#'                         c('gear', '<5')),
#'                    mtcars,
#'                    append=FALSE);
#' 
#' @export checkDataIntegrity
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
                              rmarkdownOutput=rmarkdownOutput,
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
                             rmarkdownOutput=rmarkdownOutput,
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
    varNames <- grep(x[1], names(dat), value=TRUE, perl=TRUE);
    if (length(varNames) == 0) {
      dataIntegrityLog <- addToLog(fullLog=dataIntegrityLog,
                                   showLog=!silent,
                                   paste0("No variables in the dataframe match regular expression '",
                                          ifelse(rmarkdownOutput, "<pre>--", ""),
                                          x[1],
                                          ifelse(rmarkdownOutput, "--</pre>", ""),
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
    
    return(dat);
  }
}
