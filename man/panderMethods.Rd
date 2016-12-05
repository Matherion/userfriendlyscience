\name{userfriendlysciencePanderMethods}
\alias{userfriendlysciencePanderMethods}
\alias{pander.associationMatrix}
\alias{pander.crossTab}
\alias{pander.dataShape}
\alias{pander.descr}
\alias{pander.examine}
\alias{pander.examineBy}
\alias{pander.freq}
\alias{pander.frequencies}
\alias{pander.meanDiff}
\alias{pander.normalityAssessment}
\alias{pander.oneway}
\alias{pander.regr}
\title{
userfriendlyscience methods to pander objects
}
\description{
These methods try to provide output that's ready for R Markdown. Note that they are not
all documented; most of them are quite straightforward.
}
\usage{
\method{pander}{freq}(x, ...)
\method{pander}{meanDiff}(x, digits=x$digits, powerDigits=x$digits + 2, ...)
\method{pander}{normalityAssessment}(x, headerPrefix = "#####", suppressPlot = FALSE, ...)
\method{pander}{dataShape}(x, digits=x$input$digits, extraNotification=TRUE, ...)
\method{pander}{associationMatrix}(x, info = x$input$info, file = x$input$file, ...)
\method{pander}{crossTab}(x, digits = x$input$digits,
                          pValueDigits=x$input$pValueDigits, ...)
\method{pander}{oneway}(x, digits = x$input$digits,
                        pvalueDigits=x$input$pvalueDigits,
                        headerStyle = "**",
                        na.print="", ...)
\method{pander}{regr}(x, digits=x$input$digits,
                      pvalueDigits=x$input$pvalueDigits, ...)
\method{pander}{descr}(x, headerPrefix = "", headerStyle = "**", ...)
\method{pander}{examine}(x, headerPrefix = "", headerStyle = "**",
                         secondaryHeaderPrefix = "", secondaryHeaderStyle =
                         "*", ...)
\method{pander}{examineBy}(x, headerPrefix = "", headerStyle = "**",
                           secondaryHeaderPrefix = "", secondaryHeaderStyle =
                           "*", tertairyHeaderPrefix = "--> ",
                           tertairyHeaderStyle = "", separator = paste0("\n\n",
                           repStr("-", 10), "\n\n"), ...)
\method{pander}{frequencies}(x, prefix = "###", ...)
}
\arguments{
  \item{x}{
The object to print.
}
  \item{digits}{
The number of significant digits to print.
}
  \item{powerDigits}{
Number of digits to use when printing the power.
}
  \item{headerPrefix, secondaryHeaderPrefix, tertairyHeaderPrefix, prefix}{
  Prefix for headers, can be used to output headers for pandoc using R Markdown
  by specifying e.g. '####' for a level 4 header.
}
  \item{headerStyle, secondaryHeaderStyle, tertairyHeaderStyle}{
  A character value to pre- and append to the header. This can be used to
  make the header appear bold ('**') or italic ('*') when not using an
  actual header (see \code{headerPrefix}).
}
  \item{separator}{
  Separator to show between sections of output.
}
  \item{suppressPlot}{
  Whether to suppress printing plots.
}
  \item{pValueDigits}{
Output to produce; see /code{/link{rMatrix}} for details.
}
  \item{info, file}{
Output to produce and file to write to; see /code{/link{associationMatrix}} for details.
}
  \item{extraNotification}{
Whether an extra notification about the type of skewness and kurtosis returned by dataShape is shown.
}
  \item{pvalueDigits}{
The number of digits to show for p-values; smaller p-values will be shown as
<.001 or <.0001 etc.
}
  \item{na.print}{
What to print for missing values, for example for a oneway anova table.
}
  \item{...}{
Additional arguments that are passed on to the print functions when it is called.
}
}
\value{
These printing methods use \code{\link{cat}}, \code{\link{cat0}}, and \code{\link{grid.draw}} to print stuff.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}

\examples{

userfriendlyscience:::pander.oneway(oneway(y=ChickWeight$weight, x=ChickWeight$Diet));

}
\keyword{ utilities }
