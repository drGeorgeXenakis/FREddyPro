\name{plotQC}
\alias{plotQC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plotQC
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotQC(data, var = "co2_flux", qc_var = NULL, xvar = "DOY", lines = FALSE, legendSide = NULL, ylab = var, xlab = "DOY", col = c(1, 2, 3, 4, 5, 6, 7, 8, 9), pch = c(16, 17, 18, 19, 20, 21, 22, 23, 24), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{qc_var}{
%%     ~~Describe \code{qc_var} here~~
}
  \item{xvar}{
%%     ~~Describe \code{xvar} here~~
}
  \item{lines}{
%%     ~~Describe \code{lines} here~~
}
  \item{legendSide}{
%%     ~~Describe \code{legendSide} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
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
function (data, var = "co2_flux", qc_var = NULL, xvar = "DOY", 
    lines = FALSE, legendSide = NULL, ylab = var, xlab = "DOY", 
    col = c(1, 2, 3, 4, 5, 6, 7, 8, 9), pch = c(16, 17, 18, 19, 
        20, 21, 22, 23, 24), ...) 
{
    if (is.null(qc_var)) {
        qc = paste("qc_", var, sep = "")
    }
    else {
        qc = qc_var
    }
    plot(data[[var]] ~ data[[xvar]], col = ifelse(data[[qc]] == 
        0, col[1], ifelse(data[[qc]] == 1, col[2], ifelse(data[[qc]] == 
        2, col[3], ifelse(data[[qc]] == 3, col[4], ifelse(data[[qc]] == 
        4, col[5], ifelse(data[[qc]] == 5, col[6], ifelse(data[[qc]] == 
        6, col[7], ifelse(data[[qc]] == 7, col[8], ifelse(data[[qc]] == 
        8, col[9], ifelse(data[[qc]] == 9, col[10], 1)))))))))), 
        pch = ifelse(data[[qc]] == 0, pch[1], ifelse(data[[qc]] == 
            1, pch[2], ifelse(data[[qc]] == 2, pch[3], ifelse(data[[qc]] == 
            3, pch[4], ifelse(data[[qc]] == 4, pch[5], ifelse(data[[qc]] == 
            5, pch[6], ifelse(data[[qc]] == 6, pch[7], ifelse(data[[qc]] == 
            7, pch[8], ifelse(data[[qc]] == 8, pch[9], ifelse(data[[qc]] == 
            9, pch[10], 1)))))))))), ylab = ylab, xlab = xlab, 
        ...)
    if (lines) {
        lines(data[[var]] ~ data[[xvar]], ...)
    }
    if (!is.null(legendSide)) {
        l = list(`0` = "QC=0", `1` = "QC=1", `2` = "QC=2", `3` = "QC=3", 
            `4` = "QC=4", `5` = "QC=5", `6` = "QC=6", `7` = "QC=7", 
            `8` = "QC=8", `9` = "QC=9")
        m = as.vector(sort(unique(data[[qc]])))
        txt = c(do.call("cbind", l[as.character(m)]))
        legend(legendSide, txt, col = c(col[1], col[2], col[3], 
            col[4], col[5], col[6], col[7], col[8], col[9]), 
            pch = c(pch[1], pch[2], pch[3], pch[4], pch[5], pch[6], 
                pch[7], pch[8], pch[9]), horiz = TRUE, bty = "n")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
