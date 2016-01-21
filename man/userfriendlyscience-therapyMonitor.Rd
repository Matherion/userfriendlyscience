\name{therapyMonitor}
\alias{therapyMonitor}
\alias{therapyMonitor.multi}
\title{
therapyMonitor & therapyMonitor.multi
}
\description{
therapyMonitor & therapyMonitor.multi are useful for simple n-of-1 designs,
and were written to make it easy for therapists or other practitioners
to get some insight into the effects of their treatments.
}
\usage{
therapyMonitor(dat = NULL, design="AB", statistic="|A-B|",
               conditionColumn = NULL, variableColumn = NULL,
               timeColumn = NULL, conditionMoment = NULL,
               limit=NULL, lines=NULL, ylab=NULL, xlab=NULL,
               outputFile = NULL, outputFormats = c('svg', 'png'),
               plotTitle = "therapyMonitor results",
               plotWidth=25, plotHeight=15)
therapyMonitor.multi(dat = NULL,
                     variableColumn = NULL, conditionColumn = NULL,
                     conditionMoment = NULL, minLevels = 5,
                     outputFiles = FALSE, outputFilePath = getwd(),
                     outputFormats = c('svg', 'png'), silent=FALSE,
                     ...)
}
\arguments{
  \item{dat}{
  A dataframe containing the variables to analyse. If not dataframe is
  specified, get \code{\link{getData}} function is used to present a
  dialog to the user.
}
  \item{design}{
  The design to use; see \code{\link{pvalue.systematic}} in the
  \code{\link{SCRT-package}} for more information. Note that currently,
  this function always assumes an "AB" design; changing this only changes
  the way \code{\link{pvalue.systematic}} is called.
}
  \item{statistic}{
  The statistic to use; see \code{\link{pvalue.systematic}} in the
  \code{\link{SCRT-package}} for more information. Note that currently,
  this function always assumes the "|A-B|" statistic; changing this only
  changes the way \code{\link{pvalue.systematic}} is called.

}
  \item{conditionColumn}{
  The name of the variable containing, for each measurement, the condition,
  or the phase of the treatment. This variable should normally only have
  two levels (e.g. 'A' and 'B'), indicating when the treatment changed from
  condition 'A' to condition 'B'.
}
  \item{variableColumn}{
  For \code{therapyMonitor}, this must be a single value: the name of the
  variable to analyse as dependent variable. For \code{therapyMonitor.multi},
  this can be a vector, in which case all the specified variables are
  analysed sequentially. In any case, the variable(s) specified here must
  have the 'interval' measurement level (i.e. be roughly continuous). For
  \code{therapyMonitor.multi}, if this argument is empty, all variables
  are used, provided they have at least \code{minLevels} levels.
}
  \item{timeColumn}{
  The variable containing the time (datetime) of each measurement moment.
  If not specified in R's POSIXct format, the function tries to guess
  whether SPSS, SAS, or Stata timestamps were specified, and tries to
  convert. If the timeColumn isn't specified, the function will assume
  that all measurements were equidistant, and they'll simply be assigned
  consecutive numbers als measurement moments.
}
  \item{conditionMoment}{
  The conditionMoment argument provides an alternative method of specifying
  when the condition changed; this can be the number of the first
  measurement in the new (second) condition/phase. For example, if the
  treatment started after the 6th measurement, this can be specified by
  passing 'conditionMoment=7'.
}
  \item{limit}{
  The minimum number of consecutive measurements that has to be available
  within one condition/phase to enable the analysis (see
  \code{\link{pvalue.systematic}}).
}
  \item{lines}{
  Which lines in the \code{dat} dataframe to use.
}
  \item{ylab, xlab}{
  Labels to use when creating the plots.
}
  \item{outputFile}{
  If not NULL, the filename to write the plot to. Note that this filename
  should not include the extension - this is appended based on the
  \code{outputFormats} argument.
  
}
  \item{outputFormats}{
  Which format to use for the plot or plots to export.
}
  \item{plotTitle}{
  The title for the plot.
}
  \item{plotWidth, plotHeight}{
  The size of the plot (in centimeters).
}
  \item{minLevels}{
  The minimum number of levels that a variable in the datafile
  has to have before it's included in the analyses.
}
  \item{outputFiles}{
  Whether to export the plots and regular output to files.
}
  \item{outputFilePath}{
  If \code{outputFiles} is TRUE, the path where to store the output files.
}
\item{silent}{
  Whether to suppress messages about progress etc.
}
  \item{...}{
  Additional arguments to \code{therapyMonitor.multi} are passed on to
  \code{therapyMonitor}.
}
}
\details{

This function started as a wrapper to the \code{\link{pvalue.systematic}}
function in the \code{\link{SCRT-package}}, but it now also does some extra
stuff.

}
\value{

For therapyMonitor, an object with the input and several output variables,
as well as a plot. For therapyMonitor.multi, an object containing several
therapyMonitor objects, as well as collated output.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
### Explore and plot the weight of a chick in the ChickWeight dataset
therapyMonitor(ChickWeight, variableColumn='weight',
               conditionMoment=6, lines=1:12);
}
\keyword{ utilities }