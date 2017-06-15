\name{setCaptionNumbering}
\alias{setCaptionNumbering}
\title{
Convenience function for numbered captions in knitr (and so, RMarkdown)
}
\description{
This function makes it easy to tell knitr (and so RMarkdown) to use numbered captions of any type.
}
\usage{
setCaptionNumbering(captionName = "tab.cap",
                    prefix = ":Table \%s: ",
                    suffix = "",
                    captionBefore = FALSE,
                    romanNumeralSetting = "counter_roman",
                    optionName = paste0("setCaptionNumbering_", captionName),
                    resetCounterTo = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{captionName}{
  The name of the caption; this is used both as unique identifier for the counter, and to set the caption text (included between the prefix and suffix) in the chunk options.
}
  \item{prefix}{
  The text to add as prefix before the action caption; this will typically include '\%s\%' which will be replaced by the number of this caption.
}
  \item{suffix}{
  The text to add as suffix after the action caption; this can also include '\%s\%' which will be replaced by the number of this caption. Together with the \code{prefix}, this can also be used to enclose the caption in html.
}
  \item{captionBefore}{
  Whether the caption should appear before or after the relevant chunk output.
}
  \item{romanNumeralSetting}{
  The name of the option (should be retrievable with \code{\link{getOption}}) where it's configured whether to use Roman (TRUE) or Latin (FALSE) numerals. FALSE is assumed if this option isn't set.
}
  \item{optionName}{
  The name of the option to use to retrieve and set the counter. This can be used, for example, to have multiple caption types use the same counter.
}
  \item{resetCounterTo}{
  If not \code{NULL} and numeric, the counter will start at this number.
}
}
\value{
This function returns nothing, but instead sets the appropriate \code{\link{knit_hooks}}. Or rather, just one hook.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
\dontrun{
  setCaptionNumbering(captionName='tab.cap',
                      prefix = ":Table \%s: ");
}
}
\keyword{ utils }
