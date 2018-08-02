\name{facComAnalysis}
\alias{facComAnalysis}
\title{
Wrapper for psych's factor analysis and principal components analysis functions
}
\description{
This function is meant as a wrapper to the excellent \code{\link{fa}} and \code{\link{principal}} functions from the \code{\link{psych}} package. This function was developed to provide a more familiar interface for users coming from SPSS.
}
\usage{
facComAnalysis(data,
               items = NULL,
               nfactors = NULL,
               fm = "pa",
               rotate = "default",
               covar = FALSE,
               screeplot = TRUE,
               SMC = FALSE,
               maskLoadingsUnder = NULL,
               showUnrotatedLoadings = FALSE,
               factorCorrelations = "score.cor",
               ...)
}
\arguments{
  \item{data}{
  The dataframe containt the items to analyse.
}
  \item{items}{
  The items to analyse; if none are specified, all items in the dataframe are analyses.
}
  \item{nfactors}{
  The number of factors to extract. The default, \code{NULL}, applies the Kaiser criterion (like SPSS does).
}
  \item{fm}{
  The method to use. Specify \code{pca} to conduct a principal components analysis (PCA; using \code{\link{principal}}), or one of the methods accepted by \code{\link{psych}}'s \code{\link{fa}} to conduct a factor analysis. The default (\code{pa}) performs an exploratory factor analysis using principal axis factoring. Note that SPSS' default is to perform PCA, which is usually wrong in psychological research. In PCA, the components (called factors in factor analysis) are constructed to optimally explain all item variance (i.e. the variances if a covariance matrix is analysed, and \code{1} if a correlation matrix is analysed, see argument \code{covar} below) as well as the associations between items (i.e. the covariances or correlations, depending on the value of \code{covar}). This means that variance that is unique to an item is considered important. In factor analysis, the factors (called components in PCA) are constructed to optimally explain only the variation shared between all items. This means that only a part of the variance of each item is explained: therefore, the diagonal of the correlation matrix (or covariance matrix) is replaced by an estimate of the so-called communality: that portion of variance that each item shared with other items. Of course, this is unknown: therefore, an iterative procedure is used where some initial value is used as communality estimate (and placed on the diagonal in the first iteration). Which value is used as initial estimate can be specified in the \code{SMC} argument. In any case, this difference means that in factor analysis, the unique variance in each item (the uniqueness or unicity) is assumed to reflect measurement error. This is consistent with latent variable models, which are usually what underlie operationalisations of psychological constructs. These operationalisations usually consist of items (or 'indicators') where scores registered for each item are assumed to reflect (more accurately, be caused by) a psychological construct (the latent variable). Therefore, in such cases, PCA is inappropriate. If the operationalisation is an index rather than a scale (see Peters, 2014, for a discussion of this distinction), however, factor analysis is inappropriate, and PCA should be used. This difference nicely coincides with the distinction between reflective models (where factor analysis is appropriate) and formative models (where PCA is appropriate): see Freid (2017) for an introduction.
}
  \item{rotate}{
Which rotation to use. The default, aptly called \code{default}, uses the \code{\link{psych}} defaults: \code{oblimin} for factor analysis and \code{varimax} for PCA.
}
  \item{covar}{
Whether to analyse the covariance matrix or the correlation matrix. If the items are to be aggregated without first standardizing them (which is by far the more common approach), the covariance matrix should be analysed. However, if the items are going to be standardized before aggregation, the correlation matrix will be used. However, because analysing the correlation matrix is the default setting in both SPSS and \code{\link{psych}}'s functions, it is also retained as default here.
}
  \item{screeplot}{
Whether to generate and show a screeplot.
}
  \item{SMC}{
The SMC argument can be used to specify, for factor analysis, whether to start the initial iteration with 1 (\code{SMC=FALSE}), the squared multiple correlations or R squared values obtained when regressing each item on all the others (\code{SMC=TRUE}), or custom values which then have to be provided in \code{SMC} in a numeric vector (of equal length to the number of items).
}
  \item{maskLoadingsUnder}{
Whether to show all factor loadings (if set to \code{NULL} or anything non-numeric) or whether to mask (hide) factor loadings under a given value, e.g. only showing factor loadings of .3 or higher.
}
  \item{showUnrotatedLoadings}{
Whether to only show the rotated factor loadings, or the original (unrotated) factor loadings as well.
}
  \item{factorCorrelations}{
Whether to show the factor correlations from \code{score.cor} ("The correlation matrix of course coded (unit weighted) factor score estimates, if they were to be found, based upon the loadings matrix rather than the weights matrix.", see \code{\link{fa}}) or from \code{r.scores} ("The correlations of the factor score estimates using the specified model, if they were to be found.", see \code{\link{fa}}) from the objects produced by  the \code{\link{psych}} functions.
}
  \item{\dots}{
  Any additional arguments are passed to \code{\link{psych}}'s \code{\link{fa}} and \code{\link{principal}} functions.
}
}
\value{
This function returns an object with the original \code{\link{psych}} function objects in the \code{intermediate} sub-object, and the primary results such as the factor loading and the plot in the \code{output} sub-object.
}
\references{
Fried, E. I. (2017). What are psychological constructs? On the nature and statistical modeling of emotions, intelligence, personality traits and mental disorders. \emph{Health Psychology Review}, 11(2), 130-134. \url{https://doi.org/10.1080/17437199.2017.1306718}

Peters, G.-J. Y. (2014). The alpha and the omega of scale reliability and validity: why and how to abandon Cronbach's alpha and the route towards more comprehensive assessment of scale quality. \emph{European Health Psychologist}, 16(2), 56-69. \url{http://ehps.net/ehp/index.php/contents/article/download/ehp.v16.i2.p56/1}
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{psych}}, \code{\link{fa}}, \code{\link{principal}} and \code{\link{reliability}}
}
\examples{
### Generate data frame to use for the example
dat <- as.data.frame(apply(mtcars, 2, scale));

### Conduct principal components analysis
facComAnalysis(dat, fm='pca');

### Conduct factor analysis, and mask
### all factor loadings under .3
facComAnalysis(dat, maskLoadingsUnder = .3);

}
\keyword{ univar }
