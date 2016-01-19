\name{didacticPlot}
\alias{didacticPlot}
\alias{didacticPlotTheme}
\title{
didacticPlot
}
\description{
didacticPlot is useful for making ggplot2 plots of distributions of t, F, Chi^2, and Pearson r, showing a given value, and shading the arie covering the more extreme values. didacticPlotTheme is the basic theme.
}
\usage{
didacticPlot(foundValue, statistic, df1, df2 = NULL,
             granularity = 1000, xLim = NULL, yLab = NULL,
             lineCol = "red", lineSize=1,
             surfaceCol = "red", textMarginFactor = 20,
             sided="two")
didacticPlotTheme(base_size = 14, base_family = "")
}
\arguments{
  \item{foundValue}{
  The value to indicate (the 'found' value).
}
  \item{statistic}{
  One of "r", "t", "f" or "chisq".
}
  \item{df1, df2}{
  The degrees of freedom; only use df1 for the r, t and chi^2 test; for the F-test, use df1 for the degrees of freedom of the denominator and df2 for the degrees of freedom of the numerator.
}
  \item{granularity}{
  Steps to use for x-axis.
}
  \item{xLim}{
  Vector; minimum and maximum values on x axis.
}
  \item{yLab}{
  Label on y axis.
}
  \item{lineCol}{
  Colour of density line.
}
  \item{lineSize}{
  Size of density line.
}
  \item{surfaceCol}{
  Colour of coloured surface area.
}
  \item{textMarginFactor}{
  Used to calculate how close to the vertical line text labels should appear.
}
  \item{sided}{
  Whether to make a plot for a 2-sided or 1-sided test.
}
  \item{base_size, base_family}{
  Passed on to the grey ggplot theme.
}
}
\value{
didacticPlot returns an object that contains the plot in the $plot element.
}
\examples{
didacticPlot(1, statistic='chisq', df1=2);

didacticPlot(1, statistic='t', df1=40);

didacticPlot(2.02, statistic='t', df1=40, textMarginFactor=25);

### Two sample t-test for n1 = n2 = 250, showing
### p-value of 5%
# a<-didacticPlot(1.96, statistic='t', df1=498);

}
\keyword{ utilities }