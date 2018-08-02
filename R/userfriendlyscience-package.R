

#' associationMatrix Helper Functions
#' 
#' These objects contain a number of settings and functions for
#' associationMatrix.
#' 
#' 
#' @aliases computeStatistic_t computeStatistic_r computeStatistic_f
#' computeStatistic_chisq computeEffectSize_d computeEffectSize_r
#' computeEffectSize_etasq computeEffectSize_v associationMatrixESDefaults
#' associationMatrixStatDefaults computeEffectSize_omegasq
#' @param var1 One of the two variables for which to compute a statistic or
#' effect size
#' @param var2 The other variable for which to compute the statistic or effect
#' size
#' @param conf.level The confidence for the confidence interval for the effect
#' size
#' @param bootstrap Whether to bootstrap to estimate the confidence interval
#' for Cramer's V.  If FALSE, the Fisher's Z conversion is used.
#' @param samples If bootstrapping, the number of samples to generate (of
#' course, more samples means more accuracy and longer processing time).
#' @param var.equal Whether to test for equal variances ('test'), assume
#' equality ('yes'), or assume unequality ('no'). See \code{\link{meanDiff}}
#' for more information.
#' @param \dots Any additonal arguments are sometimes used to specify exactly
#' how statistics and effect sizes should be computed.
#' @return
#' 
#' associationMatrixStatDefaults and associationMatrixESDefaults contain the
#' default functions from computeStatistic and computeEffectSize that are
#' called (see the help file for associationMatrix for more details).
#' 
#' The other functions return an object with the relevant statistic or effect
#' size, with a confidence interval for the effect size.
#' 
#' For computeStatistic, this object always contains: \item{statistic}{The
#' relevant statistic} \item{statistic.type}{The type of statistic}
#' \item{parameter}{The degrees of freedom for this statistic} \item{p.raw}{The
#' p-value of this statistic for NHST} And in addition, it often contains
#' (among other things, sometimes): \item{object}{The object from which the
#' statistics are extracted}
#' 
#' For computeEffectSize, this object always contains: \item{es}{The point
#' estimate for the effect size} \item{esc.type}{The type of effect size}
#' \item{ci}{The confidence interval for the effect size} And in addition, it
#' often contains (among other things, sometimes): \item{object}{The object
#' from which the effect size is extracted}
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{meanDiff}}, \code{\link{associationMatrix}}
#' @keywords utilities bivar
#' @examples
#' 
#' 
#' computeStatistic_f(Orange$Tree, Orange$circumference)
#' computeEffectSize_etasq(Orange$Tree, Orange$circumference)
#' 
#' 
NULL





#' Basic SPSS translation functions
#' 
#' Basic functons to make working with R easier for SPSS users: getData and
#' getDat provide an easy way to load SPSS datafiles, and exportToSPSS to write
#' to a datafile and syntax file that SPSS can import; filterBy and useAll
#' allow easy temporary filtering of rows from the dataframe; mediaan and modus
#' compute the median and mode of ordinal or numeric data.
#' 
#' 
#' @aliases basicSPSStranslationFunctions getData getDat exportToSPSS filterBy
#' useAll mediaan modus
#' @param filename,file It is possible to specify a path and filename to load
#' here. If not specified, the default R file selection dialogue is shown.
#' \code{file} is still available for backward compatibility but will
#' eventually be phased out.
#' @param errorMessage The error message that is shown if the file does not
#' exist or does not have the right extension; "[defaultErrorMessage]" is
#' replaced with a default error message (and can be included in longer
#' messages).
#' @param applyRioLabels Whether to apply the labels supplied by Rio. This will
#' make variables that has value labels into factors.
#' @param use.value.labels Only useful when reading from SPSS files: whether to
#' read variables with value labels as factors (TRUE) or numeric vectors
#' (FALSE).
#' @param to.data.frame Only useful when reading from SPSS files: whether to
#' return a dataframe or not.
#' @param stringsAsFactors Whether to read strings as strings (FALSE) or
#' factors (TRUE).
#' @param silent Whether to suppress potentially useful information.
#' @param ...  Additional options, passed on to the function used to import the
#' data (which depends on the extension of the file).
#' @param dfName The name of the dataframe to create in the parent environment.
#' @param backup Whether to backup an object with name \code{dfName}, if one
#' already exists in the parent environment.
#' @param dat Dataframe to process: for filterBy, dataframe to filter rows
#' from; for useAll, dataframe to restore ('unfilter').
#' @param datafile The name of the data file, a comma separated values file
#' that can be read into SPSS by using the code file.
#' @param codefile The name of the code file, the SPSS syntax file that can be
#' used to import the data file.
#' @param savfile The name of the SPSS format .sav file (alternative for
#' writing a datafile and a codefile).
#' @param fileEncoding The encoding to use to write the files.
#' @param newLinesInString A string to replace newlines with (SPSS has problems
#' reading newlines).
#' @param expression Logical expression determining which rows to keep and
#' which to drop. Can be either a logical vector or a string which is then
#' evaluated. If it's a string, it's evaluated using 'with' to evaluate the
#' expression using the variable names.
#' @param replaceOriginalDataframe Whether to also replace the original
#' dataframe in the parent environment. Very messy, but for maximum
#' compatibility with the 'SPSS way of doing things', by default, this is true.
#' After all, people who care about the messiness/inappropriateness of this
#' function wouldn't be using it in the first place :-)
#' @param envir The environment where to create the 'backup' of the unfiltered
#' dataframe, for when useAll is called and the filter is deactivated again.
#' @param replaceFilteredDataframe Whether to replace the filtered dataframe
#' passed in the 'dat' argument (see replaceOriginalDataframe).
#' @param vector For mediaan and modus, the vector for which to find the median
#' or mode.
#' @return
#' 
#' getData returns the imported dataframe, with the filename from which it was
#' read stored in the 'filename' attribute.
#' 
#' getDat is a simple wrapper for \code{getData()} which creates a dataframe in
#' the parent environment, by default with the name 'dat'. Therefore, calling
#' \code{getDat()} in the console will allow the user to select a file, and the
#' data from the file will then be read and be available as 'dat'. If an object
#' with \code{dfName} (i.e. 'dat' by default) already exists, it will be backed
#' up with a warning. \code{getDat()} therefore returns nothing.
#' 
#' mediaan returns the median, or, in the case of a factor where the median is
#' in between two categories, both categories.
#' 
#' modus returns the mode.
#' @note getData() currently can't read from LibreOffice or OpenOffice files.
#' There doesn't seem to be a platform-independent package that allows this.
#' Non-CRAN package ROpenOffice from OmegaHat should be able to do the trick,
#' but fails to install (manual download and installation using
#' http://www.omegahat.org produces "ERROR: dependency 'Rcompression' is not
#' available for package 'ROpenOffice'" - and manual download and installation
#' of RCompression produces "Please define LIB_ZLIB; ERROR: configuration
#' failed for package 'Rcompression'"). If you have any suggestions, please let
#' me know!
#' @keywords utilities file univar
#' @examples
#' 
#' 
#' \dontrun{
#' ### Open a dialogue to read an SPSS file
#' getData();
#' }
#' 
#' ### Get a median and a mode
#' mediaan(c(1,2,2,3,4,4,5,6,6,6,7));
#' modus(c(1,2,2,3,4,4,5,6,6,6,7));
#' 
#' ### Create an example dataframe
#' (exampleDat <- data.frame(x=rep(8, 8), y=rep(c(0,1), each=4)));
#' ### Filter it, replacing the original dataframe
#' (filterBy(exampleDat, "y=0"));
#' ### Restore the old dataframe
#' (useAll(exampleDat));
#' 
#' 
NULL





#' conversion functions
#' 
#' These are a number of functions to convert statistics and effect size
#' measures from/to each other.
#' 
#' Note that by default, the behavior of \code{convert.d.to.r} depends on the
#' sample size (see Bruce, Kromrey & Ferron, 1998).
#' 
#' @aliases convert convert.r.to.t convert.t.to.r convert.b.to.t convert.t.to.p
#' convert.f.to.p convert.f.to.d convert.chisq.to.p convert.chisq.to.V
#' convert.d.to.logodds convert.d.to.r convert.d.to.t convert.d.to.variance
#' convert.etasq.to.cohensf convert.etasq.to.r convert.f.to.etasq
#' convert.f.to.omegasq convert.fisherz.to.r convert.logodds.to.d
#' convert.logodds.to.r convert.or.to.d convert.or.to.r convert.r.to.d
#' convert.r.to.fisherz convert.r.to.p convert.t.to.d convert.V.to.r
#' convert.cohensf.to.omegasq convert.cohensfsq.to.omegasq convert.means.to.d
#' convert.ncf.to.omegasq convert.omegasq.to.cohensf
#' convert.omegasq.to.cohensfsq convert.omegasq.to.f convert.percentage.to.se
#' @param chisq,cohensf,cohensfsq,d,etasq,f,logodds,means,omegasq,or,p,r,t,z
#' The value of the relevant statistic or effect size.
#' @param ncf The value of a noncentrality parameter of the F distribution.
#' @param n,n1,n2,N,ns The number of observations that the r or t value is
#' based on, or the number of observations in each of the two groups for an
#' anova, or the total number of participants when specifying a noncentrality
#' parameter.
#' @param df,df1,df2 The degrees of freedrom for that statistic (for F, the
#' first one is the numerator (i.e. the effect), and the second one the
#' denominator (i.e. the error term).
#' @param proportion The proportion of participants in each of the two groups
#' in a t-test or anova. This is used to compute the sample size in each group
#' if the group sizes are unknown.  Thus, if you only provide df1 and df2 when
#' converting an F value to a Cohen's d value, equal group sizes are assumed.
#' @param b The value of a regression coefficient.
#' @param se,sds The standard error of standard errors of the relevant
#' statistic (e.g. of a regression coefficient) or variables.
#' @param minDim The smallest of the number of columns and the number of rows
#' of the crosstable for which the chisquare is translated to a Cramer's V
#' value.
#' @param lower.tail For the F and chisquare distributions, whether to get the
#' probability of the lower or upper tail.
#' @param akfEq8 When converting Cohen's \emph{d} to \emph{r}, for small sample
#' sizes, bias is introduced when the commonly suggested formula is used
#' (Aaron, Kromrey & Ferron, 1998). Therefore, by default, this function uses
#' different equations depending on the sample size (for n < 50 and for n >
#' 50). When \code{akfEq8} is set to TRUE or FALSE, the corresponding action is
#' taken; when \code{akfEq8} is not logical (i.e. TRUE or FALSE), the function
#' depends on the sample size.
#' @param var.equal Whether to compute the value of \emph{t} or Cohen's
#' \emph{d} assuming equal variances ('yes'), unequal variances ('no'), or
#' whether to test for the difference ('test').
#' @return
#' 
#' The converted value as a numeric value.
#' @author Gjalt-Jorn Peters and Peter Verboon
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @references Aaron, B. Kromrey J. D. & Ferron, J. (1998) \emph{Equating
#' "r"-based and "d"-based Effect Size Indices: Problems with a Commonly
#' Recommended Formula.} Paper presented at the Annual Meeting of the Florida
#' Educational Research Association (43rd, Orlando, FL, November 2-4, 1998).
#' @keywords utilities
#' @examples
#' 
#' convert.t.to.r(t=-6.46, n=200);
#' convert.r.to.t(r=-.41, n=200);
#' 
#' ### Compute some p-values
#' convert.t.to.p(4.2, 197);
#' convert.chisq.to.p(5.2, 3);
#' convert.f.to.p(8.93, 3, 644);
#' 
#' ### Convert d to r using both equations
#' convert.d.to.r(d=.2, n1=5, n2=5, akfEq8 = FALSE);
#' convert.d.to.r(d=.2, n1=5, n2=5, akfEq8 = TRUE);
#' 
NULL





#' Functions to preprocess determinant structures
#' 
#' These functions are used in conjunction with the
#' \code{\link{determinantStructure}} family of funtions to conveniently work
#' with determinant structures.
#' 
#' This family of functions will be explained more in detail in a forthcoming
#' paper.
#' 
#' @aliases detStructAddVarLabels detStructAddVarNames detStructComputeProducts
#' detStructComputeScales
#' @param determinantStructure The \code{\link{determinantStructure}} object.
#' @param varLabelDf The variable label dataframe as generated by
#' \code{\link{processLSvarLabels}}.  It is also possible to specify 'homemade'
#' dataframe, in which case the column names have to specified (see the next
#' arguments).
#' @param varNameCol The name of the column of the \code{varLabelDf} that
#' contains the variable name. Only needs to be changed from the default value
#' if \code{varLabelDf} is not a dataframe as produced by
#' \code{\link{processLSvarLabels}}.
#' @param leftAnchorCol The name of the column of the \code{varLabelDf} that
#' contains the left anchor. Only needs to be changed from the default value if
#' \code{varLabelDf} is not a dataframe as produced by
#' \code{\link{processLSvarLabels}}.
#' @param rightAnchorCol The name of the column of the \code{varLabelDf} that
#' contains the right anchor. Only needs to be changed from the default value
#' if \code{varLabelDf} is not a dataframe as produced by
#' \code{\link{processLSvarLabels}}.
#' @param subQuestionCol The name of the column of the \code{varLabelDf} that
#' contains the subquestion. Only needs to be changed from the default value if
#' \code{varLabelDf} is not a dataframe as produced by
#' \code{\link{processLSvarLabels}}.
#' @param questionTextCol The name of the column of the \code{varLabelDf} that
#' contains the question text. Only needs to be changed from the default value
#' if \code{varLabelDf} is not a dataframe as produced by
#' \code{\link{processLSvarLabels}}.
#' @param names A character vector with the variable names. These are matched
#' against the regular expressions as specified in the
#' \code{\link{determinantStructure}} object, and any matches will be stored in
#' the \code{\link{determinantStructure}} object.
#' @param dat The dataframe containing the data; the variables names specified
#' in \code{names} (when calling \code{detStructAddVarNames}) must be present
#' in this dataframe.
#' @param append Whether to only return the products or scales, or whether to
#' append these to the dataframe and return the entire dataframe.
#' @param separator The separator to use when constructing the scale variables
#' names.
#' @return \code{detStructAddVarLabels} and \code{detStructAddVarNames} just
#' change the \code{\link{determinantStructure}} object;
#' \code{detStructComputeProducts} and \code{detStructComputeScales} return
#' either the dataframe with the new variables appended (if \code{append} =
#' \code{TRUE}) or just a dataframe with the new variables (if \code{append} =
#' \code{FALSE}).
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{determinantStructure}}, \code{\link{determinantVar}},
#' \code{\link{subdeterminants}}, \code{\link{subdeterminantProducts}},
#' \code{\link{detStructCIBER}}
#' @references (Forthcoming)
#' @keywords utilities
#' @examples
#' 
#' ### Generate a silly determinant structure
#' detStruct <- determinantStructure('This makes no sense',
#'                                   list('mpg',
#'                                        behaviorRegEx = 'mpg'),
#'                                   determinantVar("Proximal determinant",
#'                                                  "t",
#'                                                  determinantVar("Determinant",
#'                                                                 "p",
#'                                                                 subdeterminants("Subdeterminants",
#'                                                                                 "a"))));
#' 
#' ### Add the variable names
#' detStructAddVarNames(detStruct, names(mtcars));
#' 
#' ### Add the determinant scale variable to the dataframe
#' mtcarsPlus <- detStructComputeScales(detStruct, mtcars);
#' 
#' ### Show its presence
#' names(mtcarsPlus);
#' mean(mtcarsPlus$mpg_Determinant);
#' 
NULL





#' Convenience functions for ggplots based on multiple variables
#' 
#' These are convenience functions to quickly generate plots for multiple
#' variables, with the variables in the y axis.
#' 
#' 
#' @aliases ggEasyPlots ggEasyRidge ggEasyBar
#' @param data The dataframe containing the variables.
#' @param items The variable names (if not provided, all variables will be
#' used).
#' @param labels Labels can optionally be provided; if they are, these will be
#' used instead of the variable names.
#' @param sortByMean Whether to sort the variables by mean value.
#' @param xlab,ylab The labels for the x and y axes.
#' @param scale_fill_function The function to pass to \code{\link{ggplot}} to
#' provide the colors of the bars.
#' @param fontColor,fontSize The color and size of the font used to display the
#' labels
#' @param labelMinPercentage The minimum percentage that a category must reach
#' before the label is printed (in whole percentages, i.e., on a scale from 0
#' to 100).
#' @param showInLegend What to show in the legend in addition to the values;
#' nothing ("\code{none}"), the frequencies ("\code{freq}"), the percentages
#' ("\code{perc}"), or both ("\code{both}"). This is only used if only one
#' variable is shown in the plot; afterwise, after all, the absolute
#' frequencies and percentages differ for each variable.
#' @param biAxisLabels This can be used to specify labels to use if you want to
#' use labels on both the left and right side. This is mostly useful when
#' plotting single questions or semantic differentials. This must be a list
#' with two character vectors, \code{leftAnchors} and \code{rightAnchors},
#' which must each have the same length as the number of items specified in
#' \code{items}. See the examples for, well, examples.
#' @return A \code{\link{ggplot}} plot is returned.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{geom_ridgeline}}, \code{\link{geom_bar}}
#' @keywords hplot
#' @examples
#' 
#' ggEasyBar(mtcars, c('gear', 'carb'));
#' ggEasyRidge(mtcars, c('disp', 'hp'));
#' 
#' ### When plotting single questions, if you want to show the anchors:
#' ggEasyBar(mtcars, c('gear'),
#'           biAxisLabels=list(leftAnchors="Fewer",
#'                             rightAnchors="More"));
#' 
#' ### Or for multiple questions (for e.g. semantic differentials):
#' ggEasyBar(mtcars, c('gear', 'carb'),
#'           biAxisLabels=list(leftAnchors=c("Fewer", "Lesser"),
#'                             rightAnchors=c("More", "Greater")));
#' 
NULL





#' The distribution of Omega Squared
#' 
#' These functions use some conversion to and from the \emph{F} distribution to
#' provide the Omega Squared distribution.
#' 
#' The functions use \code{\link{convert.omegasq.to.f}} and
#' \code{\link{convert.f.to.omegasq}} to provide the Omega Squared
#' distribution.
#' 
#' @aliases domegaSq pomegaSq qomegaSq romegaSq
#' @param x,q Vector of quantiles, or, in other words, the value(s) of Omega
#' Squared.
#' @param p Vector of probabilites (\emph{p}-values).
#' @param df1,df2 Degrees of freedom for the numerator and the denominator,
#' respectively.
#' @param n Desired number of Omega Squared values.
#' @param populationOmegaSq The value of Omega Squared in the population; this
#' determines the center of the Omega Squared distribution. This has not been
#' implemented yet in this version of \code{userfriendlyscience}. If anybody
#' has the inverse of \code{\link{convert.ncf.to.omegasq}} for me, I'll happily
#' integrate this.
#' @param lower.tail logical; if TRUE (default), probabilities are the
#' likelihood of finding an Omega Squared smaller than the specified value;
#' otherwise, the likelihood of finding an Omega Squared larger than the
#' specified value.
#' @return \code{domegaSq} gives the density, \code{pomegaSq} gives the
#' distribution function, \code{qomegaSq} gives the quantile function, and
#' \code{romegaSq} generates random deviates.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{convert.omegasq.to.f}},
#' \code{\link{convert.f.to.omegasq}}, \code{\link{df}}, \code{\link{pf}},
#' \code{\link{qf}}, \code{\link{rf}}
#' @keywords univar
#' @examples
#' 
#' ### Generate 10 random Omega Squared values
#' romegaSq(10, 66, 3);
#' 
#' ### Probability of findings an Omega Squared
#' ### value smaller than .06 if it's 0 in the population
#' pomegaSq(.06, 66, 3);
#' 
#' 
NULL





#' userfriendlyscience methods to pander objects
#' 
#' These methods try to provide output that's ready for R Markdown. Note that
#' they are not all documented; most of them are quite straightforward.
#' 
#' 
#' @aliases userfriendlysciencePanderMethods pander.associationMatrix
#' pander.crossTab pander.dataShape pander.descr pander.examine
#' pander.examineBy pander.freq pander.frequencies pander.meanDiff
#' pander.normalityAssessment pander.oneway pander.regr
#' @param x The object to print.
#' @param digits The number of significant digits to print.
#' @param powerDigits Number of digits to use when printing the power.
#' @param headerPrefix,secondaryHeaderPrefix,tertairyHeaderPrefix,prefix Prefix
#' for headers, can be used to output headers for pandoc using R Markdown by
#' specifying e.g. '####' for a level 4 header.
#' @param headerStyle,secondaryHeaderStyle,tertairyHeaderStyle A character
#' value to pre- and append to the header. This can be used to make the header
#' appear bold ('**') or italic ('*') when not using an actual header (see
#' \code{headerPrefix}).
#' @param separator Separator to show between sections of output.
#' @param suppressPlot Whether to suppress printing plots.
#' @param pValueDigits Output to produce; see /code/linkrMatrix for details.
#' @param info,file Output to produce and file to write to; see
#' /code/linkassociationMatrix for details.
#' @param extraNotification Whether an extra notification about the type of
#' skewness and kurtosis returned by dataShape is shown.
#' @param pvalueDigits The number of digits to show for p-values; smaller
#' p-values will be shown as <.001 or <.0001 etc.
#' @param na.print What to print for missing values, for example for a oneway
#' anova table.
#' @param ...  Additional arguments that are passed on to the print functions
#' when it is called.
#' @return These printing methods use \code{\link{cat}}, \code{\link{cat0}},
#' and \code{\link{grid.draw}} to print stuff.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utilities
#' @examples
#' 
#' 
#' userfriendlyscience:::pander.oneway(oneway(y=ChickWeight$weight, x=ChickWeight$Diet));
#' 
#' 
NULL





#' userfriendlyscience print methods
#' 
#' These methods print the userfriendlyscience objects. Note that they are not
#' all documented; most of them are quite straightforward.
#' 
#' 
#' @aliases userfriendlysciencePrintMethods print.dlvPlot print.freq
#' print.meanConfInt print.meanDiff print.meanDiff.multi
#' print.normalityAssessment print.oddsratio print.powerHist print.rMatrix
#' print.scaleDiagnosis print.scaleStructure print.sdConfInt
#' print.testRetestAlpha print.testRetestCES print.testRetestReliability
#' print.parallelSubscales print.dataShape print.didacticPlot
#' print.scatterMatrix print.associationMatrix print.crossTab print.confIntV
#' print.cohensdCI print.CramersV print.oneway print.posthocTGH
#' print.scaleInspection print.regr print.processOpenSesameIAT
#' print.processOpenSesameIAT.log print.descr print.therapyMonitor
#' print.therapyMonitor.multi print.asymmetricalScatterMatrix print.fullFact
#' print.confIntOmegaSq print.examine print.examineBy print.frequencies
#' print.power.htest.ufs print.regrInfluential print.nnc print.fanova
#' print.genlog print.ggProportionPlot print.logRegr print.piecewiseRegr
#' @param x The object to print.
#' @param digits The number of significant digits to print.
#' @param nsmall The minimum number of digits to the right of the decimal point
#' in formatting real/complex numbers in non-scientific formats. Allowed values
#' are 0 <= nsmall <= 20.
#' @param transposed,t Whether the frequency object should be printed
#' transposed (this can be useful for blind users).
#' @param powerDigits Number of digits to use when printing the power.
#' @param output,env.LaTeX,pboxWidthMultiplier,colNames,pValueDigits Output to
#' produce; see /code/linkrMatrix for details.
#' @param type,info,file Output to produce and file to write to; see
#' /code/linkassociationMatrix for details.
#' @param extraNotification Whether an extra notification about the type of
#' skewness and kurtosis returned by dataShape is shown.
#' @param pvalueDigits The number of digits to show for p-values; smaller
#' p-values will be shown as <.001 or <.0001 etc.
#' @param printPlot Whether to also print the plot.
#' @param na.print What to print for missing values, for example for a oneway
#' anova table.
#' @param show To override the 'show' argument, which is sometimes used to
#' inhibit printing of extensive information with e.g. many plots, which is
#' useful for some functions that, for example, primarily generate a PDF.
#' @param row.names Whether to print rownames.
#' @param ...  Addition arguments that are passed on to the print functions
#' when it's called.
#' @return These printing methods return nothing, but print stuff.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utilities
#' @examples
#' 
#' 
#' print(sdConfInt(sd=4, n=20));
#' 
#' print(oneway(y=ChickWeight$weight, x=ChickWeight$Diet), na.print="[NO VALUE]");
#' 
#' 
NULL





#' Computations for successful randomization
#' 
#' \code{prob.randomizationSuccess} computes the probability that two groups
#' are equivalent given a specific sample size, number of nuisance variables,
#' and definition of 'equivalence' (in terms of the Cohen's d expressing the
#' maximum acceptable difference between the groups on any of the nuisance
#' variables).
#' 
#' \code{pwr.randomizationSuccess} computes the sample size required to make
#' randomization succeed in a specified proportion of the studies with a
#' two-cell design. 'Success' is defined as the two groups differing at most
#' with a specified effect size on any of a given number or nuisance variables.
#' 
#' For more details, see Peters & Gruijters (2017).
#' 
#' @aliases pwr.randomizationSuccess prob.randomizationSuccess
#' @param n The sample size.
#' @param dNonequivalence The maximum difference between the two groups that is
#' deemed acceptable.
#' @param pRandomizationSuccess The desired probability that the randomization
#' procedure succeeded in generating two equivalent groups (i.e. differing at
#' most with \code{dNonequivalence}).
#' @param nNuisanceVars The number of nuisance variables that the researchers
#' assumes exists.
#' @return For \code{prob.randomizationSuccess}, the probability that the two
#' groups are equivalent. The function is vectorized, so returns either a
#' vector of length one, a vector of length > 1, a matrix, or an array.
#' 
#' For \code{pwr.randomizationSuccess}, the required sample size. The function
#' is vectorized, so returns either a vector of length one, a vector of length
#' > 1, a matrix, or an array.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{dCohensd}}
#' @references Peters, G. J.-Y. & Gruijters, S. Why your experiments fail:
#' sample sizes required for randomization to generate equivalent groups as a
#' partial solution to the replication crisis (2017). https://doi.org/
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ### To be on the safe side: sample size required to
#' ### obtain 95% likelihood of success when assuming
#' ### 100 nuisance variables exist.
#' pwr.randomizationSuccess(dNonequivalence = 0.2,
#'                          pRandomizationSuccess = 0.95,
#'                          nNuisanceVars = 100);
#' 
#' ### Living on the edge:
#' pwr.randomizationSuccess(dNonequivalence = 0.2,
#'                          pRandomizationSuccess = 0.60,
#'                          nNuisanceVars = 10);
#' 
#' ### For those with quite liberal ideas of 'equivalence':
#' pwr.randomizationSuccess(dNonequivalence = 0.5,
#'                          pRandomizationSuccess = 0.95,
#'                          nNuisanceVars = 100);
#' 
#' ### And these results can be checked with
#' ### prob.randomizationSuccess:
#' prob.randomizationSuccess(1212, .2, 100);
#' prob.randomizationSuccess(386, .2, 10);
#' prob.randomizationSuccess(198, .5, 100);
#' 
#' ### Or in one go:
#' prob.randomizationSuccess(n=c(198, 386, 1212), c(.2, .5), c(10, 100));
#' 
#' 
NULL





#' rnwString functions
#' 
#' The rnwString functions make knitting PFDs a bit more userfriendly.
#' 
#' The sanitizeLatexString function sanitizes a LaTeX string by escaping
#' special characters. It is strongly based on the function described on
#' http://stackoverflow.com/questions/5406071/r-sweave-latex-escape-variables-to-be-printed-in-latex
#' by Aaron Rendahl.
#' 
#' 
#' @aliases rnwString hasLaTeX sanitizeLatexString rnwString.initiate
#' rnwString.terminate rnwString.generate
#' @param studyName The name of the study - used as the title of the PDF.
#' @param authorName The name of the author(s) - also inserted on title page of
#' the PDF.
#' @param docClassArgs Default arguments for the document class in LaTeX. For
#' example, to use landscape pages, this should be 'a4paper,landscape,11pt'.
#' @param newPage Whether to end the initiation string with a newpage command.
#' This can be set to false if you want to add more information on the first
#' page(s).
#' @param pageMargins Margin of the pages in millimeters.
#' @param rnwString The rnwString to terminate or (after termination) generate.
#' @param rnwPath The path where the temporary files (.rnw, .tex, etc) should
#' be created. Use forward slashes. Note: the last character should not be a
#' slash!
#' @param fileName The filename to use for the temporary files. Omit the
#' extension!
#' @param pdfLatexPath The path to PdfLaTex. This file is part of a LaTeX
#' installation that creates a pdf out of a .tex file.
#' 
#' In Windows, you can download (portable) MikTex from
#' http://miktex.org/portable. You then decide yourself where to install
#' MikTex; pdflatex will end up in a subfolder 'miktex\bin', so if you
#' installed MikTex in, for example, 'C:\Program Files\MikTex', the total path
#' becomes 'C:\Program Files\MikTex\miktex\bin'. Note that R uses slashes
#' instead of backslashes to separate folders, so in this example, pdfLatexPath
#' should be 'C:/Program Files/MikTex/miktex/bin'
#' 
#' In MacOS, you can install MacTex from http://tug.org/mactex/ By default,
#' pdflatex ends up in folder '/user/texbin', which is what pdfLatexPath should
#' be in that default case.
#' 
#' In Ubuntu, you can install TexLive base by using your package manager to
#' install texlive-latex-base, or using the terminal: 'sudo apt-get install
#' texlive-latex-base' In ubuntu, by default pdflatex ends un in folder
#' '/usr/bin', which is what pdfLatexPath should be in that default case.
#' @param envir The environment where to evaluate the expressions (normally the
#' environment where the function is called).
#' @param str The character string to sanitize.
#' @return
#' 
#' rnwString.initiate starts an rnwString; rnwString.terminate closes it; and
#' rnwString.generate takes an rnwString and creates a pdf.
#' 
#' sanitizeLatexString returns the sanitized string.
#' 
#' hasLaTeX checks pdfLatexPath to make sure pdflatex or pdflatex.exe exists.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utilities
#' @examples
#' 
#' 
#' ### sanitize a string
#' newString <- sanitizeLatexString('this is a tilde: ~.');
#' newString;
#' ### newString is now: "this is a tilde: \~."
#' 
#' 
NULL





#' The distribution of R squared (as obtained in a regression analysis)
#' 
#' These functions use the beta distribution to provide the R Squared
#' distribution.
#' 
#' The functions use \code{\link{convert.omegasq.to.f}} and
#' \code{\link{convert.f.to.omegasq}} to provide the Omega Squared
#' distribution.
#' 
#' @aliases dRsq pRsq qRsq rRsq
#' @param x,q Vector of quantiles, or, in other words, the value(s) of R
#' Squared.
#' @param p Vector of probabilites (\emph{p}-values).
#' @param nPredictors The number of predictors.
#' @param sampleSize The sample size.
#' @param n The number of R Squared values to generate.
#' @param populationRsq The value of R Squared in the population; this
#' determines the center of the R Squared distribution. This has not been
#' implemented yet in this version of \code{userfriendlyscience}. If anybody
#' knows how to do this and lets me know, I'll happily integrate this of
#' course.
#' @param lower.tail logical; if TRUE (default), probabilities are the
#' likelihood of finding an R Squared smaller than the specified value;
#' otherwise, the likelihood of finding an R Squared larger than the specified
#' value.
#' @return \code{dRsq} gives the density, \code{pRsq} gives the distribution
#' function, \code{qRsq} gives the quantile function, and \code{rRsq} generates
#' random deviates.
#' @note These functions are based on the Stack Exchange (Cross Validated) post
#' at
#' \url{http://stats.stackexchange.com/questions/130069/what-is-the-distribution-of-r2-in-linear-regression-under-the-null-hypothesis}.
#' Thus, the credits go to Alecos Papadopoulos, who provided the answer that
#' was used to write these functions.
#' @author Gjalt-Jorn Peters (based on a CrossValidated answer by Alecos
#' Papadopoulos)
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{dbeta}}, \code{\link{pbeta}}, \code{\link{qbeta}},
#' \code{\link{rbeta}}
#' @keywords univar
#' @examples
#' 
#' ### Generate 10 random R Squared values
#' ### with 2 predictors and 100 participants
#' rRsq(10, 2, 100);
#' 
#' ### Probability of finding an R Squared of
#' ### .15 with 4 predictors and 100 participants
#' pRsq(.15, 4, 100, lower.tail = FALSE);
#' 
#' ### Probability of finding an R Squared of
#' ### .15 with 15 predictors and 100 participants
#' pRsq(.15, 15, 100, lower.tail=FALSE);
#' 
#' 
NULL





#' Verbal and physical aggression scores from Singh et al. (2007)
#' 
#' This is a dataset originally described in Singh et al. (2007), and digitized
#' by Rumen Manolov using plot digitizer software and used to illustrate a
#' number of single case design analysis approaches in Manolov & Moeyaert
#' (2016). It is also used by Verboon & Peters (2017) to illustrate the
#' \code{\link{piecewiseRegr}} and the \code{\link{genlog}} functions.
#' 
#' 
#' @name Singh
#' @docType data
#' @format A data frame with 56 observations on the following 6 variables.
#' \describe{ \item{list("tier")}{A numeric subject identifier.}
#' \item{list("id")}{A character subject identifier (i.e. a name).}
#' \item{list("time")}{An index of the measurement moment.}
#' \item{list("phase")}{A dummy variable indicating the phase of the
#' experiment: 0 means that treatment has not yet started, 1 means that
#' treatment has started.} \item{list("score_physical")}{The subjects' scores
#' on physical aggression.} \item{list("score_verbal")}{The subjects' scores on
#' verbal aggression.} }
#' @seealso \code{\link{piecewiseRegr}} and \code{\link{genlog}} both contain
#' examples using this dataset.
#' @references Singh, N. N., Lancioni, G. E., Winton, A. S., Adkins, A. D.,
#' Wahler, R. G., Sabaawi, M., & Singh, J. (2007). Individuals with mental
#' illness can control their aggressive behavior through mindfulness training.
#' \emph{Behavior Modification, 31}(3), 313-328.
#' https://doi.org/10.1177/0145445506293585
#' 
#' Manolov, R., & Moeyaert, M. (2017). How Can Single-Case Data Be Analyzed?
#' Software Resources, Tutorial, and Reflections on Analysis. \emph{Behavior
#' Modification, 41}(2), 179-228. https://doi.org/10.1177/0145445516664307
#' 
#' Verboon, P. & Peters, G.-J. Y. (2017) Applying the generalised logistic
#' model in SCD to deal with ceiling effects. \emph{PsyArXiv} http://INSERTLINK
#' @source See Rumen Manolov's Open Science Framework repository at
#' \url{https://osf.io/t6ws6} for the tutorial and the original dataset.
#' @keywords datasets
#' @examples
#' 
#' ### To load the data, use:
#' data(Singh);
#' 
NULL





#' testRetestSimData is a simulated dataframe used to demonstrate the
#' testRetestAlpha coefficient function.
#' 
#' This dataset contains the true scores of 250 participants on some variable,
#' and 10 items of a scale administered twice (at t0 and at t1).
#' 
#' This dataset was generated with the code in the reliabilityTest.r test
#' script.
#' 
#' @name testRetestSimData
#' @docType data
#' @format A data frame with 250 observations on the following 21 variables.
#' \describe{ \item{list("trueScore")}{The true scores}
#' \item{list("t0_item1")}{Score on item 1 at test}
#' \item{list("t0_item2")}{Score on item 2 at test}
#' \item{list("t0_item3")}{Score on item 3 at test}
#' \item{list("t0_item4")}{Score on item 4 at test}
#' \item{list("t0_item5")}{Score on item 5 at test}
#' \item{list("t0_item6")}{Score on item 6 at test}
#' \item{list("t0_item7")}{Score on item 7 at test}
#' \item{list("t0_item8")}{Score on item 8 at test}
#' \item{list("t0_item9")}{Score on item 9 at test}
#' \item{list("t0_item10")}{Score on item 10 at test}
#' \item{list("t1_item1")}{Score on item 1 at retest}
#' \item{list("t1_item2")}{Score on item 2 at retest}
#' \item{list("t1_item3")}{Score on item 3 at retest}
#' \item{list("t1_item4")}{Score on item 4 at retest}
#' \item{list("t1_item5")}{Score on item 5 at retest}
#' \item{list("t1_item6")}{Score on item 6 at retest}
#' \item{list("t1_item7")}{Score on item 7 at retest}
#' \item{list("t1_item8")}{Score on item 8 at retest}
#' \item{list("t1_item9")}{Score on item 9 at retest}
#' \item{list("t1_item10")}{Score on item 10 at retest} }
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords datasets
#' @examples
#' 
#' data(testRetestSimData);
#' head(testRetestSimData);
#' hist(testRetestSimData$t0_item1);
#' cor(testRetestSimData);
#' 
NULL





#' Data originally published with therapyMonitor
#' 
#' This dataset was originally published along with a Dutch language article
#' that described the \code{\link{therapyMonitor}} function. This version only
#' contains the aggregated scales.
#' 
#' This dataset is an n-of-1 dataset collected during a series of therapy
#' sessions.
#' 
#' @name therapyMonitorData
#' @docType data
#' @format A data frame with 38 observations on the following 12 variables.
#' \describe{ \item{list("time")}{The measurement moment as stored by Google
#' Forms.} \item{list("datetime")}{The measurement moment converted to POSIXct,
#' R's time format.} \item{list("measurementNumber")}{The rank (number) of each
#' measurement.} \item{list("positiveAffect")}{The positive affect scale.}
#' \item{list("negativeAffect")}{The negative affect scale.}
#' \item{list("selfEsteem")}{A self esteem scale.} \item{list("intimacy")}{An
#' intimacy scale.} \item{list("erectionMasturbation")}{Erection when
#' masturbating.} \item{list("erectionPartnerSex")}{Erection while having sex
#' with partner.} \item{list("experienceMasturbation")}{Experience when
#' masturbating.} \item{list("experiencePartnerSex")}{Experience while having
#' sex with partner.} \item{list("erectionCombined")}{Aggregated scale of both
#' erection experience scales.} }
#' @source van Lankveld, J., Leusink, P., & Peters, G.-J. Y. (2017).
#' Therapie-monitoring in een blended online en face-to-face behandeling van
#' een jonge man met situatieve erectieproblemen. \emph{Tijdschrift voor
#' Seksuologie, 41}, 15-22.
#' @keywords datasets
#' @examples
#' 
#' data(therapyMonitorData)
#' ## maybe str(therapyMonitorData) ; plot(therapyMonitorData) ...
#' 
NULL





#' Userfriendlyscience (UFS)
#' 
#' This package contains a number of functions that serve two goals. First, to
#' make R more accessible to people migrating from SPSS by adding a number of
#' functions that behave roughly like their SPSS equivalents (also see
#' \url{http://rosettastats.com}). Second, to make a number of slightly more
#' advanced functions more user friendly to relatively novice users. The
#' package also conveniently houses a number of additional functions that are
#' intended to increase the quality of methodology and statistics in
#' psychology, not by offering technical solutions, but by shifting
#' perspectives, for example towards reasoning based on sampling distributions
#' as opposed to on point estimates.
#' 
#' \tabular{ll}{ Package: \tab userfriendlyscience\cr Type: \tab Package\cr
#' Version: \tab 0.7-1\cr Date: \tab 2018-05-01\cr License: \tab GPL (>= 3)\cr
#' }
#' 
#' Userfriendlyscience (UFS) contains a number of functions that serve two
#' goals.  First, to make R more accessible to people migrating from SPSS by
#' adding a number of functions that behave roughly like their SPSS equivalents
#' (also see \url{http://rosettastats.com} for a tool that helps with this).
#' Second, to make a number of slightly more advanced functions more user
#' friendly to relatively novice users. The package also conveniently houses a
#' number of additional functions that are intended to increase the quality of
#' methodology and statistics in psychology, not by offering technical
#' solutions, but by shifting perspectives, for example towards reasoning based
#' on sampling distributions as opposed to on point estimates.
#' 
#' The package imports functions from many other packages, which is in line
#' with its function as a 'wrapper package': UFS aims to make many existing
#' functions easier for users coming from SPSS, so sometimes a function is
#' added when it saves the user just some data preparing.
#' 
#' The package implements many solutions provided by people all over the world,
#' most from Stack Exchange (both from Cross Validated and Stack Overflow). I
#' credit these authors in the help pages of those functions and in the
#' Author(s) section of this page. If you wrote a function included here, and
#' you want me to take it out, feel free to contact me of course (also, see
#' \url{http://meta.stackoverflow.com/questions/319171/i-would-like-to-use-a-function-written-by-a-stack-overflow-member-in-an-r-packag}).
#' 
#' @name userfriendlyscience-package
#' @aliases userfriendlyscience-package userfriendlyscience ufs UFS
#' @docType package
#' @author Author: Gjalt-Jorn Peters (Open University of the Netherlands,
#' Greater Good, and Maastricht University).
#' 
#' Contributors: Peter Verboon (\code{\link{convert.omegasq.to.cohensf}},
#' \code{\link{genlog}}, and \code{\link{piecewiseRegr}}, Open University of
#' the Netherlands), Amy Chan (\code{\link{ggPie}}), Jeff Baggett
#' (\code{\link{posthocTGH}}, University of Wisconsin - La Crosse), Daniel
#' McNeish (\code{\link{scaleStructure}}, University of North Carolina), Nick
#' Sabbe (\code{\link{curfnfinder}}, Arteveldehogeschool), Douglas Bonett
#' (\code{\link{confIntR}}, \code{\link{pwr.confIntR}}, UC Santa Cruz, United
#' States), Murray Moinester (\code{\link{confIntR}},
#' \code{\link{pwr.confIntR}}, Tel Aviv University, Israel), Stefan Gruijters
#' (\code{\link{nnc}}, \code{\link{ggNNC}}, \code{\link{convert.d.to.eer}},
#' \code{\link{convert.d.to.nnc}}, \code{\link{erDataSeq}}, Maastricht
#' University), Ron Pat-El (\code{\link{logRegr}}, Open University of the
#' Netherlands), Ananda Mahto (\code{\link{multiResponse}}).
#' 
#' A number of functions in this package use code fragments that were used
#' without explicit communicating with the author (because I've been unable to
#' find contact details of the authors, or because I haven't gotten around to
#' contacting them yet). The authors of these fragments are John Fox
#' (\code{car} code in \code{\link{ggqq}}), Floo0 (\code{\link{ggqq}}), Jason
#' Aizkalns (\code{\link{ggBoxplot}}), Luke Tierney (in
#' \code{\link{pwr.cohensdCI}}, its alias \code{\link{pwr.confIntd}}, and
#' \code{\link{cohensdCI}}).
#' 
#' In addition, the function \code{escapeRegEx} from package \code{Hmisc} is
#' included and used internally to avoid importing that entire package just for
#' that function. This function was written by Charles Dupont (Department of
#' Biostatistics, Vanderbilt University). The help page was also taken from
#' that package. The \code{ad.test} function from package \code{nortest} was
#' included for the same reason; this was written by Juergen Gross. These
#' functions are not exported.
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{psych}} and \code{\link{MBESS}} contain many useful
#' functions for researchers in psychology.
#' @references Peters, G.-J. Y. (2014).
#' \href{https://ehps.net/ehp/index.php/contents/article/download/ehp.v16.i2.p56/1The
#' alpha and the omega of scale reliability and validity: why and how to
#' abandon Cronbach's alpha and the route towards more comprehensive assessment
#' of scale quality.} \emph{European Health Psychologist}, 16(2), 56-69.
#' 
#' Peters, G.-J. Y. (2018). Diamond Plots: a tutorial to introduce a
#' visualisation tool that facilitates interpretation and comparison of
#' multiple sample estimates while respecting their inaccuracy. \emph{PsyArXiv;
#' under review at Health Psychology Bulletin}. Preprint doi:
#' \href{https://doi.org/10.17605/osf.io/9w8yv10.17605/osf.io/9w8yv}
#' 
#' Peters, G.-J. Y. & Crutzen, R. (2018). Knowing exactly how effective an
#' intervention, treatment, or manipulation is and ensuring that a study
#' replicates: accuracy in parameter estimation as a partial solution to the
#' replication crisis. \emph{PsyArXiv; under review at Psychology & Health}.
#' Preprint doi:
#' \href{https://doi.org/10.17605/osf.io/cjsk210.17605/osf.io/cjsk2}
#' 
#' Crutzen, R., Peters, G.-J. Y., & Noijen, J. (2018). Using Confidence
#' Interval-Based Estimation of Relevance to Select Social-Cognitive
#' Determinants for Behavior Change Interventions. Frontiers in Public Health
#' 5:165. https://doi.org/10.3389/fpubh.2017.00165
#' 
#' Crutzen, R. (2014).
#' \href{https://ehps.net/ehp/index.php/contents/article/download/ehp.v16.i2.p70/25Time
#' is a jailer: what do alpha and its alternatives tell us about reliability?}
#' \emph{The European Health Psychologist}, 1(2), 70-74.
#' 
#' Crutzen, R., & Peters, G.-J. Y. (2015). Scale quality: alpha is an
#' inadequate estimate and factor-analytic evidence is needed first of all.
#' \emph{Health Psychology Review}. doi:
#' \href{https://doi.org/10.1080/17437199.2015.112424010.1080/17437199.2015.1124240}
#' 
#' Verboon, P. & Peters, G.-J. Y. (2018). Applying the generalized logistic
#' model in single case designs. \emph{PsyArXiv}. Preprint doi:
#' \href{https://doi.org/10.17605/osf.io/ad5eh10.17605/osf.io/ad5eh}
#' @keywords package
#' @examples
#' 
#' ### Create simple dataset
#' dat <- PlantGrowth[1:20,];
#' ### Remove third level from group factor
#' dat$group <- factor(dat$group);
#' 
#' ### Examine normality
#' normalityAssessment(dat$weight);
#' 
#' ### Compute mean difference and show it
#' meanDiff(dat$weight ~ dat$group, plot=TRUE);
#' 
#' ### Show the t-test
#' didacticPlot(meanDiff(dat$weight ~ dat$group)$t,
#'              statistic='t',
#'              df1=meanDiff(dat$weight ~ dat$group)$df);
#' 
#' ### Load data from simulated dataset testRetestSimData (which
#' ### satisfies essential tau-equivalence).
#' data(testRetestSimData);
#' 
#' ### Select some items in the first measurement
#' exampleData <- testRetestSimData[2:6];
#' 
#' \dontrun{
#' ### Show reliabilities
#' scaleStructure(dat=exampleData, ci=FALSE,
#'                omega.psych=FALSE, poly=FALSE);
#' }
#' 
#' ### Create a dichotomous variable
#' exampleData$group <- cut(exampleData$t0_item2, 2);
#' 
#' ### Show item distributions and means
#' meansDiamondPlot(exampleData);
#' 
#' ### Show a dlvPlot
#' dlvPlot(exampleData, x="group", y="t0_item1");
#' 
#' ### show a dlvPlot with less participants, showing the confidence
#' ### interval and standard error bars better
#' dlvPlot(exampleData[1:30, ], x="group", y="t0_item1");
#' 
#' 
NULL





#' userfriendlyscience basics
#' 
#' The userfriendlyscience basics functions are some very basic functions to
#' make life that little bit easier.
#' 
#' The safeRequire function checks whether a package is already installed. If
#' so, it loads the package (using require/library). If not, it first installs
#' it, and then loads it.
#' 
#' The trim function removes whitespaces from the start and end of a text
#' string.
#' 
#' The noZero function removes the first zero from a string that was originally
#' a number.
#' 
#' The formatPvalue function formats a P value, roughly according to APA style
#' guidelines. This means that the noZero is used to remove the zero preceding
#' the decimal point, and p values that would round to zero given the requested
#' number of digits are shown as e.g. p<.001.
#' 
#' The formatR function format a Pearson correlation for pretty printing (using
#' noZero).
#' 
#' The repeatStr (or repStr) function repeats a string a given number of times.
#' 
#' The ifelseObj function just evaluates a condition, returning one object if
#' it's true, and another if it's false.
#' 
#' The invertItem function 'unmirrors' an inverted item (i.e. for a 1-3 item, 1
#' becomes 3, 2 stays 2, and 3 becomes 1).
#' 
#' is.odd and is.even check whether a number is, or numbers in a vector are,
#' odd or even.
#' 
#' The infix function \%IN\% is a case-insensitive version of
#' \code{\link{%in%}}.
#' 
#' The cat0 function is to cat what paste0 is to paste; it simply makes
#' concatenating many strings without a separator easier.
#' 
#' The addToLog function adds a character vector to a log.
#' 
#' @aliases userfriendlyscienceBasics safeRequire trim noZero formatPvalue
#' formatR repeatStr repStr ifelseObj invertItem is.odd is.even
#' convertToNumeric massConvertToNumeric vecTxt vecTxtQ %IN% cat0 addToLog
#' @param packageName The name of the package, as character string.
#' @param mirrorIndex The index of the mirror to use, in case you want to
#' specify the mirror in the call (see e.g. /code/linkgetCRANmirrors()[, 1:4]
#' for an overview of these mirrors. For example, at the time of writing,
#' Antwerp is 7, Amsterdam is 60, and Auckland is 62).
#' @param str The character string to process.
#' @param values The p-values to format.
#' @param digits For formatPvalue, number of digits to round to. Numbers
#' smaller than this number will be shown as <.001 or <.0001 etc.
#' 
#' For formatR, the number of digits to use when formatting the Pearson
#' correlation.
#' @param spaces Whether to include spaces between symbols, operators, and
#' digits.
#' @param includeP Whether to include the 'p' and '='-symbol in the results
#' (the '<' symbol is always included).
#' @param r The Pearson correlation to format.
#' @param n The number of times to repeat the string.
#' @param condition Condition to evaluate.
#' @param ifTrue Object to return if the condition is true.
#' @param ifFalse Object to return if the condition is false.
#' @param item Item to invert
#' @param fullRange If provided it must be a numeric vector with the minimum
#' and the maximum of the scale. If not provided, the range function is used
#' (so, use this range argument if the scale minimum and/or maximum do not
#' occur in the data).
#' @param ignorePreviousInversion If this item has already been inverted, the
#' function will halt with an error unless it's told to ignore previous
#' inversions with this boolean.
#' @param dat,vector The dataframe of vector to process.
#' @param byFactorLabel If TRUE, convertToNumeric and massConvertToNumeric use
#' the factor labels, interpreted as character vectors, to determine the
#' numeric value, instead of the level's indices (which is what as.numeric()
#' does).
#' @param ignoreCharacter If TRUE, character vectors are ignored. If FALSE,
#' character vectors are converted (or, an attempt is made :-)).
#' @param stringsAsFactors If TRUE, strings (character vectors) in the
#' dataframe will be converted to factors (by as.data.frame, after the function
#' called lapply).
#' @param find The element(s) to look up in the vector or matrix.
#' @param table The vector or matrix in which to look up the element.
#' @param delimiter,firstDelimiter,lastDelimiter The delimiters to use for
#' respectively the middle, first \code{firstElements}, and last
#' \code{lastElements} elements.
#' @param useQuote This character string is pre- and appended to all elements;
#' so use this to quote all elements (\code{useQuote="'"}), doublequote all
#' elements (\code{useQuote='"'}), or anything else (e.g. \code{useQuote='|'}).
#' The only difference between \code{vecTxt} and \code{vecTxtQ} is that the
#' latter by default quotes the elements.
#' @param firstElements,lastElements The number of elements for which to use
#' the first respective last delimiters
#' @param lastHasPrecedence If the vector is very short, it's possible that the
#' sum of firstElements and lastElements is larger than the vector length. In
#' that case, downwardly adjust the number of elements to separate with the
#' first delimiter (\code{TRUE}) or the number of elements to separate with the
#' last delimiter (\code{FALSE})?
#' @param sep The separator to pass to \code{\link{cat}}, of course, "" by
#' default.
#' @param fullLog The full log - the character vector(s) provided are appended
#' to this character vector.
#' @param showLog Whether to \code{\link{cat}} the log.
#' @param ...  Extra arguments are passed to whatever function is wrapped (e.g.
#' \code{\link{cat}} for \code{cat0}). For addToLog, the dots are used to
#' provide character vectors that are concatenated using \code{\link{paste0}}
#' and (potentially shown and) added to the log.
#' @return safeRequire returns nothing.
#' 
#' trim, formatPvalue, noZero, formatR, and repeatStr return a string.
#' 
#' ifelseObj return an object.
#' 
#' The invertItem function returns the inverted item vector, with an attribute
#' "inverted" set to TRUE.
#' 
#' is.odd and is.even return a logical vector.
#' 
#' \%IN\% returns a logical vector of the same length as its first argument.
#' 
#' cat0 returns a string.
#' 
#' addToLog returns a string.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utilities
#' @examples
#' 
#' 
#' ### load a package
#' safeRequire('ggplot2');
#' 
#' ### trim a string
#' trim(' this is a string with whitespace in front and at the end               ');
#' ### Returns "this is a string with whitespace in front and at the end"
#' 
#' repeatStr("-", 8);
#' ### Returns "--------" (incredibly useful, no? :-))
#' 
#' tempVector <- c(1,2,3,3,2,4,3,2,1,1,3,4,5,4,3,2,2,1,1,2);
#' invertedTempVector <- invertItem(tempVector);
#' 
#' ### We can also invert it back, but then we have to override the security
#' ### that prevents accidently inverting items back.
#' invertItem(tempVector, ignorePreviousInversion=TRUE);
#' 
#' 
NULL





#' Only compute means or sums for cases with enough nonmissings
#' 
#' These functions have been written as equivalents of SPSS' \code{MEAN.x} and
#' \code{SUM.x} functions, which only compute means and sums if enough cases
#' have valid values.
#' 
#' 
#' @aliases validComputations validMeans validSums
#' @param ...  Either a dataframe or vectors for which to compute the mean or
#' sum.
#' @param requiredValidValues How many values must be valid (i.e. nonmissing)
#' to compute the mean or sum. If a number lower than 1 is provided, it is
#' interpreted as proportion, and the number of variables is computed. For
#' example, if \code{requiredValidValues=.8}, 80\% of the variables must have
#' valid values. If 'all' is specified, all values must be valid (in which case
#' the functions are equal to \code{\link{rowMeans}} and
#' \code{\link{rowSums}}).
#' @param returnIfInvalid Wat to return for cases that don't have enough valid
#' values.
#' @param silent Whether to show the number of cases that have to be valid if
#' \code{requiredValidValues} is a proportion.
#' @return A numeric vector with the resulting means or sums.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{rowMeans}}, \code{\link{rowSums}}
#' @keywords manip
#' @examples
#' 
#' validMeans(mtcars$cyl, mtcars$disp);
#' validSums(mtcars$cyl, mtcars$disp, requiredValidValues = .8);
#' 
#' ### Or specifying a dataframe
#' validSums(mtcars);
#' 
NULL



