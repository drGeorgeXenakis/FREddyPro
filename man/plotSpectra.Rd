\name{plotSpectra}
\alias{plotSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plotSpectra
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotSpectra(data, gas, xlab = "Frequency [Hz]", ylab = "Spectrum", avgT = FALSE, predicted = FALSE, type = c("o", "o", "o"), col = c(1, 2, 4), pch = c(1, 1, 1), na.value = "NaN", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{gas}{
%%     ~~Describe \code{gas} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{avgT}{
%%     ~~Describe \code{avgT} here~~
}
  \item{predicted}{
%%     ~~Describe \code{predicted} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
}
  \item{na.value}{
%%     ~~Describe \code{na.value} here~~
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
function (data, gas, xlab = "Frequency [Hz]", ylab = "Spectrum", 
    avgT = FALSE, predicted = FALSE, type = c("o", "o", "o"), 
    col = c(1, 2, 4), pch = c(1, 1, 1), na.value = "NaN", ...) 
{
    if (!is.data.frame(data)) {
        data <- readFile(data, 3, 2, na = na.value)
    }
    else {
        data <- data
    }
    column = grep(gas, names(data))
    if (length(col) == 1) 
        col = c(col, col, col)
    if (length(col) == 2) 
        col = c(col[1], col[2], col[2])
    if (length(type) == 1) 
        type = c(type, type, type)
    if (length(type) == 2) 
        type = c(type[1], type[2], type[2])
    if (length(pch) == 1) 
        pch = c(pch, pch, pch)
    if (length(pch) == 2) 
        pch = c(pch[1], pch[2], pch[2])
    plot(data[[column[1]]] ~ data[[column[[1]] - 1]], log = "xy", 
        xlab = xlab, ylab = ylab, col = col[1], type = type[1], 
        pch = pch[1], xaxt = "n", ...)
    axis(1, at = c(0.001, 0.01, 0.1, 1, 10), labels = c("0.001", 
        "0.01", "0.1", "1", "10"))
    if (avgT) {
        points(data[[column[1] - 1]] ~ data[[column[[1]] - 1]], 
            col = col[2], type = type[2], pch = pch[2], ...)
    }
    if (predicted) {
        points(data[[column[2]]] ~ data[[column[[1]] - 1]], col = col[3], 
            type = type[3], pch = pch[3], ...)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
