\name{oddsratio}
\alias{oddsratio}
\title{
oddsratio
}
\description{
The oddsratio function simply computes a point estimate and confidence interval for an odds ratio.
}
\usage{
oddsratio(x, y = NULL, conf.level = .95, digits=2)
}
\arguments{
  \item{x}{
x can be either a table (then y can be NULL) or a factor.
}
  \item{y}{
If x is a factor, y also has to be a factor; x and y are then used to create the crosstable.
}
  \item{conf.level}{
The confidence level of the confidence interval.
}
  \item{digits}{
Number of digits to round output to
}
}
\value{
The oddsratio function returns an object with the input and output.
\item{input}{List with input arguments}
\item{or}{Point estimate for odds ratio}
\item{or.ci}{Confidence interval for odds ratio}
}
\examples{
### Generate two factor vectors
treatment <- factor(c(rep(0, 33), rep(1, 45), rep(0, 63), rep(1, 21)),
                    levels=c(0,1), labels=c("no", "yes"));
survival <- factor(c(rep(0, 78), rep(1, 84)),
                   levels=c(0, 1), labels=c("no", "yes"));

### Compute and display odds ratio
oddsratio(treatment, survival);

### Or present a table
oddsratio(table(treatment, survival));
}
\keyword{ utilities }