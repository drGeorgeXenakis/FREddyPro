\name{plotDaily}
\alias{plotDaily}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plotDaily
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotDaily(x, day.x, y = NULL, day.y = NULL, median = FALSE, quantiles = FALSE, probs = c(0.05, 0.95), legend = FALSE, legendSide = NULL, legendText = NULL, type = "l", lty = c(1, 4), col = c(1, 2), cex.legend = 1, horiz.legend = TRUE, ylab = "Mean and median", xlab = "Day", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{day.x}{
%%     ~~Describe \code{day.x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{day.y}{
%%     ~~Describe \code{day.y} here~~
}
  \item{median}{
%%     ~~Describe \code{median} here~~
}
  \item{quantiles}{
%%     ~~Describe \code{quantiles} here~~
}
  \item{probs}{
%%     ~~Describe \code{probs} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
}
  \item{legendSide}{
%%     ~~Describe \code{legendSide} here~~
}
  \item{legendText}{
%%     ~~Describe \code{legendText} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{lty}{
%%     ~~Describe \code{lty} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{cex.legend}{
%%     ~~Describe \code{cex.legend} here~~
}
  \item{horiz.legend}{
%%     ~~Describe \code{horiz.legend} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (x, day.x, y = NULL, day.y = NULL, median = FALSE, quantiles = FALSE, 
    probs = c(0.05, 0.95), legend = FALSE, legendSide = NULL, 
    legendText = NULL, type = "l", lty = c(1, 4), col = c(1, 
        2), cex.legend = 1, horiz.legend = TRUE, ylab = "Mean and median", 
    xlab = "Day", ...) 
{
    if (length(type) == 1) 
        type = c(type, type, type, type)
    if (length(type) == 2) 
        type = c(type[1], type[2], type[1], type[2])
    if (length(type) == 3) 
        type = c(type[1], type[2], type[3], type[1])
    if (length(lty) == 1) 
        lty = c(lty, lty, lty, lty)
    if (length(lty) == 2) 
        lty = c(lty[1], lty[2], lty[1], lty[2])
    if (length(lty) == 3) 
        lty = c(lty[1], lty[2], lty[3], lty[1])
    if (length(col) == 1) 
        col = c(col, col, col, col)
    if (length(col) == 2) 
        col = c(col[1], col[2], col[1], col[2])
    if (length(col) == 3) 
        col = c(col[1], col[2], col[3], col[1])
    new.moccasin1 <- rgb(255, 228, 181, 225, maxColorValue = 255)
    new.moccasin2 <- rgb(255, 228, 181, 170, maxColorValue = 255)
    new.darkgrey1 <- rgb(169, 169, 169, 210, maxColorValue = 255)
    new.darkgrey2 <- rgb(169, 169, 169, 100, maxColorValue = 255)
    meanX = aggregate(x, list(day.x), mean, na.rm = TRUE)
    medianX = aggregate(x, list(day.x), median, na.rm = TRUE)
    quantileX = aggregate(x, list(day.x), quantile, probs = probs, 
        na.rm = TRUE)
    meanX = as.data.frame(meanX)
    medianX = as.data.frame(medianX)
    quantileX = as.data.frame(t(as.data.frame(t(quantileX))))
    par(xaxs = "i", yaxs = "i", mar = c(4, 5, 2, 2))
    plot(meanX[[2]] ~ meanX[[1]], lty = lty[1], type = type[1], 
        col = col[1], xlab = xlab, ylab = ylab, ...)
    if (median) {
        points(medianX[[2]] ~ medianX[[1]], col = col[2], lty = lty[2], 
            type = type[2], ...)
    }
    if (quantiles) {
        polygon(c(quantileX[[1]], rev(quantileX[[1]])), c(quantileX[[2]], 
            rev(quantileX[[3]])), col = new.moccasin1, border = NA)
        points(meanX[[2]] ~ meanX[[1]], lty = lty[1], type = type[1], 
            col = col[1], ...)
        if (median) {
            points(medianX[[2]] ~ medianX[[1]], lty = lty[2], 
                type = type[2], col = col[2], ...)
        }
    }
    if (!is.null(y)) {
        meanY = aggregate(y, list(day.y), mean, na.rm = TRUE)
        medianY = aggregate(y, list(day.y), median, na.rm = TRUE)
        quantileX = aggregate(y, list(day.y), quantile, probs = probs, 
            na.rm = TRUE)
        meanY = as.data.frame(meanY)
        medianY = as.data.frame(medianY)
        quantileX = as.data.frame(t(as.data.frame(t(quantileX))))
        points(meanY[[2]] ~ meanY[[1]], lty = lty[3], type = type[3], 
            col = col[3], ...)
        if (median) {
            points(medianY[[2]] ~ medianY[[1]], col = col[4], 
                lty = lty[4], type = type[4], ...)
        }
        if (quantiles) {
            polygon(c(quantileX[[1]], rev(quantileX[[1]])), c(quantileX[[2]], 
                rev(quantileX[[3]])), col = new.darkgrey2, border = NA)
            points(meanY[[2]] ~ meanY[[1]], lty = lty[3], col = col[3], 
                type = type[3], ...)
            if (median) {
                points(medianY[[2]] ~ medianY[[1]], lty = lty[4], 
                  col = col[4], type = type[4], ...)
            }
        }
    }
    if (legend) {
        if (is.null(legendSide)) {
            print("You must give which side to draw the legend", 
                quote = FALSE)
        }
        else {
            if (!is.null(legendText)) {
                text1 = paste("Mean", legendText[1], sep = " ")
                text2 = paste("Median", legendText[1], sep = " ")
                text3 = paste("Mean", legendText[2], sep = " ")
                text4 = paste("Median", legendText[2], sep = " ")
                legend(legendSide, c(text1, text2, text3, text4), 
                  col = c(col[1], col[2], col[3], col[4]), lty = c(lty[1], 
                    lty[2], lty[3], lty[4]), horiz = horiz.legend, 
                  cex = cex.legend, bty = "n")
            }
            else {
                legend(legendSide, c("Mean", "Median"), col = c(col[1], 
                  col[2]), lty = c(lty[1], lty[2]), horiz = FALSE, 
                  bty = "n")
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
