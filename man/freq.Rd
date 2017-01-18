\name{freq}
\alias{freq}
\alias{Frequency}
\alias{frequencies}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Frequency tables
}
\description{
Function to show frequencies in a manner similar to what SPSS' "FREQUENCIES" command does. Note that \code{frequency} is an alias for \code{freq}.
}
\usage{
freq(vector, digits = 1, nsmall=1, transposed=FALSE,
     round=1, plot=FALSE, plotTheme = theme_bw())
frequencies(..., digits = 1, nsmall = 1,
            transposed = FALSE, round = 1,
            plot = FALSE, plotTheme = theme_bw())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vector}{
  A vector of values to compute frequencies for.
}
  \item{digits}{
  Minimum number of significant digits to show in result.
}
  \item{nsmall}{
  Minimum number of digits after the decimal point to show in the result.
  }
  \item{transposed}{
  Whether to transpose the results when printing them (this can be useful for blind users).
  }
  \item{round}{
  Number of digits to round the results to (can be used in conjunction with digits to determine format of results).
  }
  \item{plot}{
  If true, a histogram is shown of the variable.
  }
  \item{plotTheme}{
  The ggplot2 theme to use.
  }
  \item{\dots}{
  The variables of which to provide frequencies
  }
}
\value{

An object with several elements, the most notable of which is:
  \item{dat}{A dataframe with the frequencies}

For \code{frequencies}, these objects are in a list of their own.

}
\examples{

### Create factor vector
ourFactor <- factor(mtcars$gear, levels=c(3,4,5),
                    labels=c("three", "four", "five"));
### Add some missing values
factorWithMissings <- ourFactor;
factorWithMissings[10] <- factorWithMissings[20] <- NA;

### Show frequencies
freq(ourFactor);
freq(factorWithMissings);

### ... Or for all of them at one
frequencies(ourFactor, factorWithMissings);

}
\keyword{ univar }
