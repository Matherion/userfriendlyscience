###########################################################
###########################################################
###
### Function to generate a PDF with some diagnostics to
### assess how the elements (usually items) in a scale
### relate to each other.
###
### File created by Gjalt-Jorn Peters. Questions? You can
### contact me through http://behaviorchange.eu.
###
###########################################################
###########################################################

### This function generates a pdf file with a report
### describing the variables.


#' scaleInspection and a number of useful helper functions
#' 
#' scaleInspection is a function to generate a PDF with information to diagnose
#' and inspect scales (aggregate measures); makeScales actually generates the
#' scales; and meanConfInt and sdConfInt provide confidence intervals for means
#' and standard deviations.
#' 
#' 
#' scaleInspection generates a PDF with useful diagnostics to assess a scale;
#' those from scaleDiagnosis and an rMatrix.
#' 
#' makeScales generates the scales and stores them in the dataframe.
#' 
#' meanConfInt and sdConfInt just compute and return a confidence interval for
#' a mean or standard deviation.
#' 
#' @aliases scaleInspection makeScales meanConfInt sdConfInt
#' @param dat Dataframe containing the items of the relevant scale
#' @param items Either a character vector with the itemnames, or, if the items
#' are organised in scales, a list of character vectors with the items in each
#' scale.
#' @param scales A list of character vectors with the items in each scale,
#' where each vectors' name is the name of the scale.
#' @param docTitle Title to use when generating the PDF.
#' @param docAuthor Author(s) to include when generating the PDF.
#' @param pdfLaTexPath The path to PdfLaTex. This file is part of a LaTeX
#' installation that creates a pdf out of a .tex file.
#' 
#' In Windows, you can download (portable) MikTex from
#' http://miktex.org/portable. You then decide yourself where to install
#' MikTex; pdflatex will end up in a subfolder 'miktex\bin', so if you
#' installed MikTex in, for example, 'C:\Program Files\MikTex', the total path
#' becomes 'C:\Program Files\MikTex\miktex\bin'. Note that R uses slashes
#' instead of backslashes to separate folders, so in this example, pdfLaTexPath
#' should be 'C:/Program Files/MikTex/miktex/bin'
#' 
#' In MacOS, you can install MacTex from http://tug.org/mactex/ By default,
#' pdflatex ends up in folder '/user/texbin', which is what pdfLaTexPath should
#' be in that default case.
#' 
#' In Ubuntu, you can install TexLive base by using your package manager to
#' install texlive-latex-base, or using the terminal: 'sudo apt-get install
#' texlive-latex-base' In ubuntu, by default pdflatex ends un in folder
#' '/usr/bin', which is what pdfLaTexPath should be in that default case.
#' @param rnwPath The path where the temporary files and the resulting PDF
#' should be stored.
#' @param filename The filename to use to save the pdf.
#' @param convertFactors Whether to convert factors to numeric vectors for the
#' analysis.
#' @param scaleReliability.ci TRUE or FALSE: whether to compute confidence
#' intervals for Cronbach's Alpha and Omega (uses bootstrapping function in
#' MBESS, takes a while).
#' @param conf.level Confidence of confidence intervals (for reliability
#' estimates (if requested with scaleReliability.ci), meand, and sd, for
#' respectively scaleInspection, meanConfInt and sdConfInt).
#' @param digits The number of digits to use in the tables.
#' @param rMatrixColsLandscape At how many columns (or rather, variables) or
#' more should rMatrices be printed landscape?
#' @param pboxWidthMultiplier Used for print.rMatrix; used to tweak the width
#' of columns in the correlation matrix.
#' @param scatterPlotBaseSize Size of one scatterplot in the scattermatrix in
#' centimeters. If the total scattermatrix becomes larger than 18 cm, it's
#' scaled down to 18 cm.
#' @param pageMargins Margins of the page in millimeters.
#' @param show Whether to show the results (or only write them to the PDF).
#' @param pval Whether to print p-values as p-values in correlation matrix.
#' @param append Whether to return the dataframe including the new variables
#' (\code{TRUE}), or a dataframe with only those new variables (\code{FALSE}).
#' @param vector Numeric vector to use when computing confidence intervals.
#' @param mean Mean to use when computing confidence intervals (when no vector
#' is provided).
#' @param sd Standard deviaton to use when computing confidence intervals (when
#' no vector is provided).
#' @param n Number of datapoints to base confidence intervals on.
#' @param se Standard errorto use when computing confidence intervals (when no
#' standard deviation or vector is provided).
#' @return
#' 
#' scaleInspection returns nothing; it just generates a PDF.
#' 
#' makeScales returns the provided dataframe, now including the new scale
#' variables.
#' 
#' meanConfInt and sdConfInt return an object, with in its 'output' list, the
#' confidence interval for a mean or standard deviation.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords utilities
#' @examples
#' 
#' 
#' \dontrun{
#' scaleInspection(mtcars, items=c('disp', 'hp', 'drat'), pdfLaTexPath="valid/path/here");
#' }
#' 
#' newDataframe <- makeScales(mtcars, list(senselessScale = c('disp', 'hp', 'drat')));
#' 
#' sdConfInt(sd=4, n=30);
#' 
#' meanConfInt(mean=5, sd=4, n=30)
#' 
#' 
#' @export scaleInspection
scaleInspection <- function(dat, items=NULL,
                            docTitle = "Scale inspection", docAuthor = "Author",
                            pdfLaTexPath, rnwPath=getwd(),
                            filename = "scaleInspection", convertFactors=TRUE,
                            scaleReliability.ci=FALSE, conf.level=.95, digits=2,
                            rMatrixColsLandscape = 6,
                            pboxWidthMultiplier = 1,
                            scatterPlotBaseSize = 4,
                            pageMargins=15, show = FALSE,
                            pval=TRUE) {

  if (is.null(items)) {
    items <- names(dat);
  }
  
  if (!is.list(items)) {
    items <- list('none' = items);
  }
  if (FALSE %in% sapply(items, is.vector)) {
    ### If this happens, that means there 's non-vector
    ### element in the list of items, which is a problem
    stop("There is a non-vector element in the list with items:\n",
         print(sapply(items, is.vector)));
  }
  if (FALSE %in% sapply(items, is.character)) {
    ### If this happens, that means there 's non-character 
    ### vector element in the list of items, which is a problem
    stop("There is a non-character vector element in the list with items.\n",
         "Here follows a list of booleans indicating which of the list elements ",
         "are character vectors (TRUE) and which aren't (FALSE):\n",
         paste0(names(lapply(items, is.character)), ': ', lapply(items, is.character), '\n'));
  }
  
  if (!hasLaTeX(pdfLaTexPath)) {
    stop('In path "', pdfLaTexPath, '", the file pdflatex.exe (Windows) or ',
         'pdflatex (MacOS or Ubuntu (Linux)) does not exist! Please ',
         'locate the file and provide its path (without the last ',
         'slash). See ?rnwString for more information.');
  }

  res <- list();
  res$items <- items;
  res$scales <- list();
  res$describe <- list();
  res$scaleDiagnosis <- list();
  res$scaleDiagnosis.errors <- list();
  res$normality.plots <- list();
  res$rnwBit <- list();
  res$normality.sampleDist <- list();
  res$normality.samplingDist <- list();
  res$rMatrix <- list();
  res$show <- show;
  
  res$rnw <- rnwString.initiate(docTitle, docAuthor,
                                docClassArgs='a4paper,portrait,10pt',
                                newPage=FALSE, pageMargins=pageMargins);
  
  ### Process each scale separately
  for (currentScale in names(items)) {
    ### Skip scales with only one item
    if (length(items[[currentScale]]) < 2) {
      res$rnwBit[[currentScale]] <-
        paste0('\\newpage\n',
               '\\section{SCALE: ',
               sanitizeLatexString(currentScale),
               '}\n',
               sanitizeLatexString(paste(items[[currentScale]], collapse=", ")),
               '\n\n\\vspace{1ex}\n',
               'Schale has less than 2 items - skipping this one.\n',
               '\\newline\n');
    }
    else {

      ### Check whether there is a non-numeric vector
      if (FALSE %in% lapply(dat[, items[[currentScale]]], 'is.numeric')) {
        ### Check whether there is a character vector (if so, abort)
        if (TRUE %in% lapply(dat[, items[[currentScale]]], 'is.character')) {
          stop("One of the supplied variables is a character vector ",
               "(i.e. contains text strings instead of numbers)!");
        }
        else if ((TRUE %in% lapply(dat[, items[[currentScale]]], 'is.factor')) && (!convertFactors)) {
          stop("One of the supplied items is a factor (instead ",
               "of a numeric vector), and convertFactors is FALSE.");
        }
        else if ((TRUE %in% lapply(dat[, items[[currentScale]]], 'is.factor')) && convertFactors) {
          dat[, items[[currentScale]]] <- data.frame(lapply(dat[, items[[currentScale]]], 'as.numeric'));
        }
        else {
          stop("One of the items is not numeric, not a character vector, ",
               "and not a factor. Normally, items should be numeric vectors!");
        }
      }
    
      res$scales[[currentScale]] <- rowMeans(dat[, items[[currentScale]]], na.rm=TRUE);
      ### Extract univariate descriptives to show
      res$describe[[currentScale]] <-
        describe(res$scales[[currentScale]])[, c('n', 'mean', 'sd', 'median', 'min', 'max', 'skew', 'kurtosis')];
      ### Generate and store plots for assessment of normality
      res$normality.plots[[currentScale]] <-
        normalityAssessment(res$scales[[currentScale]],
                            xLabel.sampleDist = 'Sample Distribution',
                            yLabel.sampleDist = 'Density',
                            xLabel.samplingDist = 'Sampling Distribution',
                            yLabel.samplingDist = 'Density',
                            samplingDistLineSize = .5,
                            normalLineSize = .2);
      ### Create dataframe with normality statistics for the
      ### sample distribution
      res$normality.sampleDist[[currentScale]] <-
        data.frame(sw = c(round(res$normality.plots[[currentScale]]$sw.sampleDist$statistic, 4),
                          round(res$normality.plots[[currentScale]]$sw.sampleDist$p.value, 4)),
                   ad = c(round(res$normality.plots[[currentScale]]$ad.sampleDist@test$statistic, 4),
                          round(res$normality.plots[[currentScale]]$ad.sampleDist@test$p.value, 4)),
                   ks = c(round(res$normality.plots[[currentScale]]$ks.sampleDist$statistic, 4),
                          round(res$normality.plots[[currentScale]]$ks.sampleDist$p.value, 4)));
      row.names(res$normality.sampleDist[[currentScale]]) <- c('value', 'p-val');
      ### Create dataframe with normality statistics for the
      ### sampling distribution
      res$normality.samplingDist[[currentScale]] <-
        data.frame(sw = c(round(res$normality.plots[[currentScale]]$sw.samplingDist$statistic, 4),
                          round(res$normality.plots[[currentScale]]$sw.samplingDist$p.value, 4)),
                   ad = c(round(res$normality.plots[[currentScale]]$ad.samplingDist@test$statistic, 4),
                          round(res$normality.plots[[currentScale]]$ad.samplingDist@test$p.value, 4)),
                   ks = c(round(res$normality.plots[[currentScale]]$ks.samplingDist$statistic, 4),
                          round(res$normality.plots[[currentScale]]$ks.samplingDist$p.value, 4)));
      row.names(res$normality.samplingDist[[currentScale]]) <- c('value', 'p-val');
      ### Generate scale diagnosis
      tryCatch(
        res$scaleDiagnosis[[currentScale]] <-
          scaleDiagnosis(dat, as.vector(items[[currentScale]]),
                         scaleReliability.ci=scaleReliability.ci,
                         conf.level=conf.level)
        , error = function(e) {
          res$scaleDiagnosis.errors[[currentScale]] <- e;
        }
      );
      ### Generate correlation table
      res$rMatrix[[currentScale]] <- rMatrix(dat, items[[currentScale]]);
      
      ### Generate the content:
      ###  - name of measure (scale)
      ###  - list of items in scale
      ###  - table with univariate descriptives of scale
      ###  - minipage with:
      ###     - plotted sample distribution
      ###     - normality statistics for sample distribution
      ###  - minipage with:
      ###     - plotted sampling distribution
      ###     - normality statistics for sampling distribution
      ###  - internal consistency coefficients
      ###  - principal component analysis
      ###  - ggpairs plot of scatterplots, correlations, and histograms
      
      res$rnwBit[[currentScale]] <-
        paste0('\\newpage\n',
               '\\section{SCALE: ',
               sanitizeLatexString(currentScale),
               '}\n',
               sanitizeLatexString(paste(items[[currentScale]], collapse=", ")),
               '\n\n\\vspace{1ex}\n',
               '<< echo=FALSE, results="asis" >>=\n',
               '  print(xtable(res$describe[["',
               currentScale,
               '"]], digits=c(0, 0, rep(digits, 7))), tabular.environment="tabular",
               print.rownames=FALSE, floating=FALSE);\n',
               '@\n',
               '\\vspace{1ex}\\begin{minipage}[t]{80mm}\n',
               '<< echo=FALSE, warning=FALSE, dev="pdf", fig.width=8/2.54, fig.height=8/2.54 >>=\n',
               'res$normality.plots[["', currentScale, '"]]$plot.sampleDist;\n',
               '@\n',
               '\\vspace{1ex}\n<< echo=FALSE, results="asis" >>=\n',
               '  print(xtable(res$normality.sampleDist[["',
               currentScale,
               '"]], digits=digits), tabular.environment="tabular",
               floating=FALSE);\n',
               '@\n',
               '\\end{minipage}%\n',
               '\\begin{minipage}[t]{80mm}\n',
               '<< echo=FALSE, warning=FALSE, dev="pdf", fig.width=8/2.54, fig.height=8/2.54 >>=\n',
               'res$normality.plots[["', currentScale, '"]]$plot.samplingDist;\n',
               '@\n',
               '\\vspace{1ex}\n<< echo=FALSE, results="asis" >>=\n',
               'print(xtable(res$normality.samplingDist[["',
               currentScale,
               '"]], digits=digits), tabular.environment="tabular",
               floating=FALSE);\n',
               '@\n',
               '\\end{minipage}%\n\\newline\n');
      
      if (is.null(res$scaleDiagnosis[[currentScale]]$scaleReliability)) {
        stop(paste0("No scaleReliability object present for scale ", currentScale, "!"));
      }
      
      if (res$scaleDiagnosis[[currentScale]]$scaleReliability$input$n.items > 2) {
        res$rnwBit[[currentScale]] <-
          paste0(res$rnwBit[[currentScale]],
                 '\\vspace{1ex}\n<< echo=FALSE, results="asis" >>=\n',
                 'cat(paste0("\n\nOmega: ", round(res$scaleDiagnosis[["',
                 currentScale,
                 '"]]$scaleReliability$output$omega, digits), "\n\nGreatest Lower Bound (GLB): ", round(res$scaleDiagnosis[["',
                 currentScale,
                 '"]]$scaleReliability$output$glb, digits), "\n\\nCronbach\'s alpha: ", round(res$scaleDiagnosis[["',
                 currentScale,
                 '"]]$scaleReliability$output$cronbach.alpha, digits), "\n\n',
                 'Eigen values: ", paste(round(res$scaleDiagnosis[["',
                 currentScale,
                 '"]]$eigen$values, digits), collapse=", "), "\n\n',
                 'Number of factors with Eigen value over 1: ", res$scaleDiagnosis[["',
                 currentScale,
                 '"]]$factors, "\n\n"));\n',
                 '@\n');
        ### Show principal component analysis
        res$rnwBit[[currentScale]] <-
          paste0(res$rnwBit[[currentScale]],
                 '\\vspace{1ex}\n\\begin{minipage}{\\linewidth}\\begin{verbatim}\n',
                 '<< echo=FALSE, results="asis" >>=\n',
                 'print(res$scaleDiagnosis[["',
                 currentScale,
                 '"]]$pca$loadings, digits=digits);\n',
                 '@\n',
                 '\\end{verbatim}\\end{minipage}\n');
  
      } else if (res$scaleDiagnosis[[currentScale]]$scaleReliability$input$n.items == 2) {
        res$rnwBit[[currentScale]] <-
          paste0(res$rnwBit[[currentScale]],
                 '\\vspace{1cm}\n<< echo=FALSE, results="asis" >>=\n',
                 '  cat(paste0("\n\nSpearman Brown coefficient: ", round(res$scaleDiagnosis[["',
                 currentScale,
                 '"]]$scaleReliability$output$spearman.brown, digits), "\n\nCronbach\'s alpha: ", round(res$scaleDiagnosis[["',
                 currentScale,
                 '"]]$scaleReliability$output$cronbach.alpha, digits), "\n\nPearson correlation: ", round(res$scaleDiagnosis[["',
                 currentScale,
                 '"]]$scaleReliability$intermediate$cor[1,2], digits), "\n\n"));\n',
                 '@\n\\vspace{1cm}');
      }
      
      ### Include correlation table;
      ### whether to print on a portrait page or
      ### on a landscape page depends on number of
      ### columns and rMatrixColsLandscape
      if (length(res$rMatrix[[currentScale]]$variables.cols) < rMatrixColsLandscape) {
        res$rnwBit[[currentScale]] <-
          paste0(res$rnwBit[[currentScale]],
                 '\n\\begin{minipage}{\\textwidth}\n\\maxsizebox{\\textwidth}{\\textheight}{\n');
      }
      else {
        res$rnwBit[[currentScale]] <-
          paste0(res$rnwBit[[currentScale]],
                 '\\begin{landscape}\n\\maxsizebox{', 297 - 2*pageMargins, 'mm}{', 210 - 2*pageMargins, 'mm}{\n');
      }
      res$rnwBit[[currentScale]] <-
        paste0(res$rnwBit[[currentScale]],
               '<< echo=FALSE, results="asis" >>=\n',
               'print(res$rMatrix[["',
               currentScale,
               '"]], digits=digits, output="LaTeX", pboxWidthMultiplier=pboxWidthMultiplier, pval=pval);\n',
               '@\n');
      if (length(res$rMatrix[[currentScale]]$variables.cols) < rMatrixColsLandscape) {
        res$rnwBit[[currentScale]] <-
          paste0(res$rnwBit[[currentScale]],
                 '}\n\\end{minipage}\n');
      }
      else {
        res$rnwBit[[currentScale]] <-
          paste0(res$rnwBit[[currentScale]],
                 '}\n\\end{landscape}\n');
      }
      
      ### The size of each panel in the scattermatrix depends
      ### on the number of items - therefore, we need to adjust
      ### the plot sizes to the number of items. This is mainly
      ### necessary because in ggpairs.print (which you can
      ### view with "getAnywhere('print.ggpairs');"), the
      ### fontsize is fixed at 15.
      ### knitr wants unit for outputsize, no unit for figure draw
      ### size (but this must be specified in inches).
      if (res$scaleDiagnosis[[currentScale]]$scaleReliability$input$n.items * scatterPlotBaseSize > 18) {
        figSizeInOutput <- 18;
      }
      else {
        figSizeInOutput <- res$scaleDiagnosis[[currentScale]]$scaleReliability$input$n.items * scatterPlotBaseSize;
      }
      ### For two items on a page (i.e. plots of roughly 9x9 cm),
      ### the labels of the plots have roughly the right size,
      ### so we multiply 9 cm with the number of items.
      figSizeToDraw <- (9 / 2.54) * res$scaleDiagnosis[[currentScale]]$scaleReliability$input$n.items;
      ### If figSizeToDraw is smaller than output size, set to output size
      if (figSizeToDraw < (figSizeInOutput / 2.54)) {
        figSizeToDraw <- figSizeInOutput / 2.54;
      }
      ### Add unit to size in output
      figSizeInOutput <- paste0(figSizeInOutput, "cm");
      
      res$rnwBit[[currentScale]] <-
        paste0(res$rnwBit[[currentScale]],
               '\\begin{minipage}{180mm}\n',
               '<< echo=FALSE, warning=FALSE, dev="pdf", fig.width=', figSizeToDraw, ', fig.height=', figSizeToDraw, ', out.width="', figSizeInOutput, '", out.height="', figSizeInOutput, '" >>=\n',
               'print(res$scaleDiagnosis[["', currentScale, '"]]$scatterMatrix);\n',
               '@\n',
               '<< echo=FALSE, results="asis" >>=\n',
               '@\n',
               '\\end{minipage}%\n');
    }
  }

  ### Combine all pages generated for each scale
  for (currentScale in names(items)) {
    res$rnwPanels <- paste(res$rnwPanels, res$rnwBit[[currentScale]]);
  }
  
  overviewCountString <-
    paste0('CONTENTS: ', length(names(items)), ' measures (scales):\n\\tableofcontents');
  
  ### Combine all pieces
  res$rnw <- paste0(res$rnw, overviewCountString,
                    "\n\\newpage\n", res$rnwPanels);
  
  ### Finalize rnwString and generate the PDF
  res$rnw <- rnwString.terminate(res$rnw);
  rnwString.generate(res$rnw, rnwPath, fileName=filename, pdfLaTexPath);
  
  ### Store result for later inspection
  class(res) <- c('scaleInspection');
  return(res);
  
}

print.scaleInspection <- function (x, show=x$show, ...) {
  if (show) {
    print(x, ...);
  }
  invisible();
}
