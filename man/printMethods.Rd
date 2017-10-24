\name{userfriendlysciencePrintMethods}
\alias{userfriendlysciencePrintMethods}
\alias{print.dlvPlot}
\alias{print.freq}
\alias{print.meanConfInt}
\alias{print.meanDiff}
\alias{print.meanDiff.multi}
\alias{print.normalityAssessment}
\alias{print.oddsratio}
\alias{print.powerHist}
\alias{print.rMatrix}
\alias{print.scaleDiagnosis}
\alias{print.scaleStructure}
\alias{print.sdConfInt}
\alias{print.testRetestAlpha}
\alias{print.testRetestCES}
\alias{print.testRetestReliability}
\alias{print.parallelSubscales}
\alias{print.dataShape}
\alias{print.didacticPlot}
\alias{print.scatterMatrix}
\alias{print.associationMatrix}
\alias{print.crossTab}
\alias{print.confIntV}
\alias{print.cohensdCI}
\alias{print.CramersV}
\alias{print.oneway}
\alias{print.posthocTGH}
\alias{print.scaleInspection}
\alias{print.regr}
\alias{print.processOpenSesameIAT}
\alias{print.processOpenSesameIAT.log}
\alias{print.descr}
\alias{print.therapyMonitor}
\alias{print.therapyMonitor.multi}
\alias{print.asymmetricalScatterMatrix}
\alias{print.fullFact}
\alias{print.confIntOmegaSq}
\alias{print.examine}
\alias{print.examineBy}
\alias{print.frequencies}
\alias{print.power.htest.ufs}
\alias{print.regrInfluential}
\alias{print.nnc}
\alias{print.fanova}
\alias{print.genlog}
\alias{print.ggProportionPlot}
\alias{print.logRegr}
\alias{print.piecewiseRegr}
\title{
userfriendlyscience print methods
}
\description{
These methods print the userfriendlyscience objects. Note that they are not
all documented; most of them are quite straightforward.
}
\usage{
\method{print}{dlvPlot}(x, ...) 
\method{print}{freq}(x, digits=x$input$digits, nsmall=x$input$nsmall,
transposed=x$input$transposed, ...)
\method{print}{meanConfInt}(x, digits=2, ...)
\method{print}{meanDiff}(x, digits=x$digits,powerDigits=x$digits + 2, ...)
\method{print}{meanDiff.multi}(x, digits=x$digits,
powerDigits=x$digits + 2, ...)
\method{print}{normalityAssessment}(x, ...)
\method{print}{oddsratio}(x, digits=x$input$digits, ...)
\method{print}{powerHist}(x, ...)
\method{print}{rMatrix}(x, digits=x$digits, output=x$output,
pValueDigits = x$pValueDigits, env.LaTeX = x$env.LaTeX,
pboxWidthMultiplier = x$pboxWidthMultiplier,
colNames = x$colNames, ...)
\method{print}{scaleDiagnosis}(x, ...)
\method{print}{scaleStructure}(x, digits=x$input$digits, ...)
\method{print}{sdConfInt}(x, digits=2, ...)
\method{print}{testRetestAlpha}(x, ...)
\method{print}{testRetestCES}(x, digits=x$input$digits, ...)
\method{print}{testRetestReliability}(x, digits=x$input$digits, ...)
\method{print}{parallelSubscales}(x, nsmall=2, ...)
\method{print}{dataShape}(x, digits=x$input$digits, extraNotification=TRUE, ...)
\method{print}{didacticPlot}(x, ...)
\method{print}{scatterMatrix}(x, ...)
\method{print}{CramersV}(x, digits = x$input$digits, ...)
\method{print}{associationMatrix}(x, type = x$input$type,
                                  info = x$input$info,
                                  file = x$input$file, ...)
\method{print}{confIntV}(x, digits = x$input$digits, ...)
\method{print}{cohensdCI}(x, ...)
\method{print}{crossTab}(x, digits = x$input$digits,
                         pValueDigits=x$input$pValueDigits, ...)
\method{print}{oneway}(x, digits = x$input$digits,
                       pvalueDigits=x$input$pvalueDigits,
                       na.print="", ...)
\method{print}{posthocTGH}(x, digits = x$input$digits, ...)
\method{print}{scaleInspection}(x, show=x$show, ...)
\method{print}{regr}(x, digits=x$input$digits,
                     pvalueDigits=x$input$pvalueDigits, ...)
\method{print}{processOpenSesameIAT}(x, ...)
\method{print}{processOpenSesameIAT.log}(x, ...)
\method{print}{descr}(x, digits = attr(x, 'digits'),
                      t = attr(x, "transpose"),
                      row.names = FALSE, ...)
\method{print}{therapyMonitor}(x, digits=2, printPlot = TRUE, ...)
\method{print}{therapyMonitor.multi}(x, ...)
\method{print}{asymmetricalScatterMatrix}(x, ...)
\method{print}{fullFact}(x, ...)
\method{print}{confIntOmegaSq}(x, ..., digits = 2)
\method{print}{examine}(x, ...)
\method{print}{examineBy}(x, ...)
\method{print}{frequencies}(x, ...)
\method{print}{power.htest.ufs}(x, digits = x$digits, ...)
\method{print}{regrInfluential}(x, ...)
\method{print}{nnc}(x, ...)
\method{print}{fanova}(x, digits = x$input$digits, ...) 
\method{print}{genlog}(x, digits = 3, ...)
\method{print}{ggProportionPlot}(x, ...)
\method{print}{logRegr}(x, digits = x$input$digits,
                        pvalueDigits = x$input$pvalueDigits, 
                        ...)
\method{print}{piecewiseRegr}(x, digits = x$input$digits, ...) 
}
\arguments{
  \item{x}{
The object to print.
}
  \item{digits}{
The number of significant digits to print.
}
  \item{nsmall}{
The minimum number of digits to the right of the decimal point in formatting real/complex numbers in non-scientific formats. Allowed values are 0 <= nsmall <= 20.
}
  \item{transposed, t}{
Whether the frequency object should be printed transposed (this can be useful for blind users).
}
  \item{powerDigits}{
Number of digits to use when printing the power.
}
  \item{output, env.LaTeX, pboxWidthMultiplier, colNames, pValueDigits}{
Output to produce; see /code{/link{rMatrix}} for details.
}
  \item{type, info, file}{
Output to produce and file to write to; see /code{/link{associationMatrix}} for details.
}
  \item{extraNotification}{
Whether an extra notification about the type of skewness and kurtosis returned by dataShape is shown.
}
  \item{pvalueDigits}{
The number of digits to show for p-values; smaller p-values will be shown as
<.001 or <.0001 etc.
}
  \item{printPlot}{
Whether to also print the plot.
}
  \item{na.print}{
What to print for missing values, for example for a oneway anova table.
}
  \item{show}{
To override the 'show' argument, which is sometimes used to inhibit printing
of extensive information with e.g. many plots, which is useful for some functions
that, for example, primarily generate a PDF.
}
  \item{row.names}{
  Whether to print rownames.
}
  \item{...}{
Addition arguments that are passed on to the print functions when it's called.
}
}
\value{
These printing methods return nothing, but print stuff.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}

\examples{

print.sdConfInt(sdConfInt(sd=4, n=20));

print(oneway(y=ChickWeight$weight, x=ChickWeight$Diet), na.print="[NO VALUE]");

}
\keyword{ utilities }
