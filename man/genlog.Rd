\name{genlog}
\alias{genlog}
\title{
Generalized Logistic Analysis
}
\description{
This function implements the generalized logistic analysis introduced in Verboon & Peters (2017). This analysis fits a logistic function (i.e. a sigmoid) to a data series. This is useful when analysing single case designs. The function enables easy customization of the main plot elements and easy saving of the plot with anti-aliasing.
}
\usage{
genlog(data,
       timeVar = 1,
       yVar = 2,
       phaseVar = NULL,
       baselineMeasurements = NULL,
       yRange = NULL,
       startX = NULL,
       startBase = NULL,
       startTop = NULL,
       startGrowthRate = NULL,
       startV = 1,
       changeInitiationBounds = NULL,
       growthRateBounds = c(-2, 2),
       baseMargin = c(0, 3),
       topMargin = c(-3, 0),
       baseBounds = NULL,
       topBounds = NULL,
       vBounds = c(1, 1),
       colors = list(bounds = viridis(4)[4],
                     curve = viridis(4)[3],
                     mid = viridis(4)[2],
                     intervention = viridis(4)[1],
                     points = "black"),
       theme = theme_minimal(),
       pointSize = 2,
       pointAlpha = 0.5,
       lineSize = 0.5,
       curveSizeMultiplier = 2,
       showPlot = TRUE,
       outputFile = NULL,
       outputWidth = 16,
       outputHeight = 16,
       ggsaveParams = list(units = "cm",
                           dpi = 300,
                           type = "cairo"))
}
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{timeVar}{
%%     ~~Describe \code{timeVar} here~~
}
  \item{yVar}{
%%     ~~Describe \code{yVar} here~~
}
  \item{phaseVar}{
%%     ~~Describe \code{phaseVar} here~~
}
  \item{baselineMeasurements}{
%%     ~~Describe \code{baselineMeasurements} here~~
}
  \item{yRange}{
%%     ~~Describe \code{yRange} here~~
}
  \item{startX}{
%%     ~~Describe \code{startX} here~~
}
  \item{startBase}{
%%     ~~Describe \code{startBase} here~~
}
  \item{startTop}{
%%     ~~Describe \code{startTop} here~~
}
  \item{startGrowthRate}{
%%     ~~Describe \code{startGrowthRate} here~~
}
  \item{startV}{
%%     ~~Describe \code{startV} here~~
}
  \item{changeInitiationBounds}{
%%     ~~Describe \code{changeInitiationBounds} here~~
}
  \item{growthRateBounds}{
%%     ~~Describe \code{growthRateBounds} here~~
}
  \item{baseMargin}{
%%     ~~Describe \code{baseMargin} here~~
}
  \item{topMargin}{
%%     ~~Describe \code{topMargin} here~~
}
  \item{baseBounds}{
%%     ~~Describe \code{baseBounds} here~~
}
  \item{topBounds}{
%%     ~~Describe \code{topBounds} here~~
}
  \item{vBounds}{
%%     ~~Describe \code{vBounds} here~~
}
  \item{colors}{
%%     ~~Describe \code{colors} here~~
}
  \item{theme}{
%%     ~~Describe \code{theme} here~~
}
  \item{pointSize}{
%%     ~~Describe \code{pointSize} here~~
}
  \item{pointAlpha}{
%%     ~~Describe \code{pointAlpha} here~~
}
  \item{lineSize}{
%%     ~~Describe \code{lineSize} here~~
}
  \item{curveSizeMultiplier}{
%%     ~~Describe \code{curveSizeMultiplier} here~~
}
  \item{showPlot}{
%%     ~~Describe \code{showPlot} here~~
}
  \item{outputFile}{
%%     ~~Describe \code{outputFile} here~~
}
  \item{outputWidth}{
%%     ~~Describe \code{outputWidth} here~~
}
  \item{outputHeight}{
%%     ~~Describe \code{outputHeight} here~~
}
  \item{ggsaveParams}{
%%     ~~Describe \code{ggsaveParams} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, timeVar = 1, yVar = 2, phaseVar = NULL, baselineMeasurements = NULL, 
    yRange = NULL, startX = NULL, startBase = NULL, startTop = NULL, 
    startGrowthRate = NULL, startV = 1, changeInitiationBounds = NULL, 
    growthRateBounds = c(-2, 2), baseMargin = c(0, 3), topMargin = c(-3, 
        0), baseBounds = NULL, topBounds = NULL, vBounds = c(1, 
        1), colors = list(bounds = viridis(4)[4], curve = viridis(4)[3], 
        mid = viridis(4)[2], intervention = viridis(4)[1], points = "black"), 
    theme = theme_minimal(), pointSize = 2, pointAlpha = 0.5, 
    lineSize = 0.5, curveSizeMultiplier = 2, showPlot = TRUE, 
    outputFile = NULL, outputWidth = 10, outputHeight = 10, ggsaveParams = list(units = "cm", 
        dpi = 300, type = "cairo")) 
{
    result <- list(input = as.list(environment()), intermediate = list(), 
        output = list())
    result$intermediate$yVarName <- ifelse(is.numeric(yVar), 
        names(data)[yVar], yVar)
    result$intermediate$timeVarName <- ifelse(is.numeric(timeVar), 
        names(data)[timeVar], timeVar)
    result$intermediate$phaseVarName <- ifelse(is.null(phaseVar), 
        "none", ifelse(is.numeric(timeVar), names(data)[phaseVar], 
            phaseVar))
    result$intermediate$GLF <- GLF <- paste(yVar, "~ Ab + (At - Ab)/ (1 + exp(-B*(", 
        timeVar, "- x0))) ^ (1/v)")
    if (is.null(baselineMeasurements) && is.null(phaseVar)) {
        stop("Provide number of measurements in pre-intervention phase, either by ", 
            "specifying the variable indicating the phase in 'phaseVar', or by ", 
            "specifying the number of baseline measurements in 'baselineMeasurements'.")
    }
    else {
        result$intermediate$baselineMeasurements <- baselineMeasurements <- ifelse(is.null(baselineMeasurements), 
            sum(data[, phaseVar] == min(data[, phaseVar])), baselineMeasurements)
    }
    result$intermediate$startX <- startX <- ifelse(is.null(startX), 
        baselineMeasurements + 4, startX)
    result$intermediate$startGrowthRate <- startGrowthRate <- ifelse(is.null(startGrowthRate), 
        0, startGrowthRate)
    result$intermediate$startBase <- startBase <- ifelse(is.null(startBase), 
        min(data[, yVar]), startBase)
    result$intermediate$startTop <- startTop <- ifelse(is.null(startTop), 
        max(data[, yVar]), startTop)
    result$intermediate$yRange <- yRange <- ifelseObj(is.null(yRange), 
        range(data[, yVar]), yRange)
    changeInitiationBounds <- ifelseObj(is.null(changeInitiationBounds), 
        c(baselineMeasurements - 1, max(data[, timeVar]) - 5), 
        changeInitiationBounds)
    result$intermediate$startingValues <- startingValues <- list(x0 = startX, 
        B = startGrowthRate, Ab = startBase, At = startTop, v = startV)
    result$intermediate$lowerBounds <- lowerBounds <- c(init = changeInitiationBounds[1], 
        grow = growthRateBounds[1], base = ifelse(is.null(baseBounds), 
            min(yRange) + baseMargin[1], baseBounds[1]), top = ifelse(is.null(topBounds), 
            max(yRange) + topMargin[1], topBounds[1]), v = vBounds[1])
    result$intermediate$upperBounds <- upperBounds <- c(init = changeInitiationBounds[2], 
        grow = growthRateBounds[2], base = ifelse(is.null(baseBounds), 
            min(yRange) + baseMargin[2], baseBounds[2]), top = ifelse(is.null(topBounds), 
            max(yRange) + topMargin[2], topBounds[2]), v = vBounds[2])
    out <- result$intermediate$nlsLM <- nlsLM(GLF, data = data, 
        start = startingValues, lower = lowerBounds, upper = upperBounds)
    result$output$maxChangeMoment <- x0 <- as.numeric(coef(out)[1])
    result$output$growthRate <- B <- as.numeric(coef(out)[2])
    result$output$base <- Ab <- as.numeric(coef(out)[3])
    result$output$top <- At <- as.numeric(coef(out)[4])
    result$output$v <- v <- as.numeric(coef(out)[5])
    Dev <- deviance(out)
    SSQtot <- sum((data[, yVar] - mean(data[, yVar]))^2)
    result$output$Rsq <- Rsq <- (SSQtot - Dev)/SSQtot
    result$output$ES1 <- ES1 <- (At - Ab)/sd(data[, yVar])
    result$output$ES2 <- ES2 <- (At - Ab)/diff(yRange)
    result$output$dat <- data.frame(Rsq = Rsq, ES1 = ES1, ES2 = ES2, 
        growthRate = B, maxChangeMoment = x0, base = Ab, top = At)
    yfit = Ab + ((At - Ab)/(1 + exp(-B * (data[, timeVar] - x0)))^(1/v))
    interventionMoment <- mean(data[c(baselineMeasurements, baselineMeasurements + 
        1), timeVar])
    result$output$plot <- plot <- ggplot(data, aes_string(x = timeVar, 
        y = yVar)) + geom_hline(yintercept = Ab, colour = colors$bounds, 
        size = lineSize) + geom_hline(yintercept = At, colour = colors$bounds, 
        size = lineSize) + geom_vline(xintercept = x0, colour = colors$mid, 
        size = lineSize) + geom_vline(xintercept = interventionMoment, 
        colour = colors$intervention, size = lineSize) + geom_point(size = pointSize, 
        alpha = pointAlpha, color = colors$points) + geom_line(data = data.frame(x = data[, 
        timeVar], y = yfit), aes_string(x = "x", y = "y"), colour = colors$curve, 
        size = lineSize * curveSizeMultiplier) + theme + labs(x = "Measurements", 
        y = "Score")
    if (!is.null(outputFile)) {
        ggsaveParameters <- c(list(filename = outputFile, plot = plot, 
            width = outputWidth, height = outputHeight), ggsaveParams)
        do.call(ggsave, ggsaveParameters)
    }
    class(result) <- "genlog"
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
