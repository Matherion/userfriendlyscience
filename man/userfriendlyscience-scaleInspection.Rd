\name{scaleInspection}
\alias{scaleInspection}
\alias{makeScales}
\alias{meanConfInt}
\alias{sdConfInt}
\title{
scaleInspection and a number of useful helper functions
}
\description{
scaleInspection is a function to generate a PDF with information to diagnose and inspect scales (aggregate measures); makeScales actually generates the scales; and meanConfInt and sdConfInt provide confidence intervals for means and standard deviations.
}
\usage{
scaleInspection(dat, items = NULL,
                docTitle = "Scale inspection", docAuthor = "Author",
                pdfLaTexPath, rnwPath=getwd(),
                filename = "scaleInspection", convertFactors=TRUE,
                scaleReliability.ci=FALSE, conf.level=.95, digits=2,
                rMatrixColsLandscape = 6,
                pboxWidthMultiplier = 1,
                scatterPlotBaseSize = 4,
                pageMargins=15, show=FALSE,
                pval=TRUE)
makeScales(dat, scales)
meanConfInt(vector=NULL, mean=NULL, sd=NULL, n=NULL, se=NULL, conf.level=.95)
sdConfInt(vector=NULL, sd=NULL, n=NULL, conf.level=.95)
}
\arguments{
  \item{dat}{
  Dataframe containing the items of the relevant scale
}
  \item{items}{
  Either a character vector with the itemnames, or, if the items are organised in scales, a list of character vectors with the items in each scale.
}
  \item{scales}{
  A list of character vectors with the items in each scale, where each vectors' name is the name of the scale.
}
  \item{docTitle}{
  Title to use when generating the PDF.
}
  \item{docAuthor}{
  Author(s) to include when generating the PDF.
}
  \item{pdfLaTexPath}{
  The path to PdfLaTex. This file is part of a LaTeX installation that creates a pdf out of a .tex file.

In Windows, you can download (portable) MikTex from
http://miktex.org/portable. You then decide yourself
where to install MikTex; pdflatex will end up in a
subfolder 'miktex\\bin', so if you installed MikTex
in, for example, 'C:\\Program Files\\MikTex', the total
path becomes 'C:\\Program Files\\MikTex\\miktex\\bin'. Note
that R uses slashes instead of backslashes to separate
folders, so in this example, pdfLaTexPath should be
'C:/Program Files/MikTex/miktex/bin'

In MacOS, you can install MacTex from http://tug.org/mactex/
By default, pdflatex ends up in folder '/user/texbin', which
is what pdfLaTexPath should be in that default case.

In Ubuntu, you can install TexLive base by using your package
manager to install texlive-latex-base, or using the terminal:
'sudo apt-get install texlive-latex-base'
In ubuntu, by default pdflatex ends un in folder '/usr/bin',
which is what pdfLaTexPath should be in that default case.
}
  \item{rnwPath}{
  The path where the temporary files and the resulting PDF should be stored.
}
  \item{filename}{
  The filename to use to save the pdf.
}
  \item{convertFactors}{
  Whether to convert factors to numeric vectors for the analysis.
}
  \item{scaleReliability.ci}{
  TRUE or FALSE: whether to compute confidence intervals for Cronbach's Alpha and Omega (uses bootstrapping function in MBESS, takes a while).
}
  \item{conf.level}{
  Confidence of confidence intervals (for reliability estimates (if requested with scaleReliability.ci), meand, and sd, for respectively scaleInspection, meanConfInt and sdConfInt).
}
  \item{digits}{
  The number of digits to use in the tables.
}
  \item{rMatrixColsLandscape}{
  At how many columns (or rather, variables) or more should rMatrices be printed landscape?
}
  \item{pboxWidthMultiplier}{
  Used for print.rMatrix; used to tweak the width of columns in the correlation matrix.
}
  \item{scatterPlotBaseSize}{
  Size of one scatterplot in the scattermatrix in centimeters. If the total scattermatrix becomes larger than 18 cm, it's scaled down to 18 cm.
}
  \item{pageMargins}{
  Margins of the page in millimeters.
}
  \item{show}{
  Whether to show the results (or only write them to the PDF).
}
  \item{pval}{
  Whether to print p-values as p-values in correlation matrix.
}
  \item{vector}{
  Numeric vector to use when computing confidence intervals.
}
  \item{mean}{
  Mean to use when computing confidence intervals (when no vector is provided).
}
  \item{sd}{
  Standard deviaton to use when computing confidence intervals (when no vector is provided).
}
  \item{n}{
  Number of datapoints to base confidence intervals on.
}
  \item{se}{
  Standard errorto use when computing confidence intervals (when no standard deviation or vector is provided).
}
}
\details{

scaleInspection generates a PDF with useful diagnostics to assess a scale; those from scaleDiagnosis and an rMatrix.

makeScales generates the scales and stores them in the dataframe.

meanConfInt and sdConfInt just compute and return a confidence interval for a mean or standard deviation.

}
\value{

scaleInspection returns nothing; it just generates a PDF.

makeScales returns the provided dataframe, now including the new scale variables.

meanConfInt and sdConfInt return an object, with in its 'output' list, the confidence interval for a mean or standard deviation.

}

\examples{

\dontrun{
scaleInspection(mtcars, items=c('disp', 'hp', 'drat'), pdfLaTexPath="valid/path/here");
}

newDataframe <- makeScales(mtcars, list(senselessScale = c('disp', 'hp', 'drat')));

sdConfInt(sd=4, n=30);

meanConfInt(mean=5, sd=4, n=30)

}
\keyword{ utilities }