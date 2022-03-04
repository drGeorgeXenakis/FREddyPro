\name{plotMonthly}
\alias{plotMonthly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plotMonthly
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotMonthly(data, var = "co2_flux", legend = FALSE, legendSide = NULL, type = "o", col = 1, lty = 1, yaxt.out = NULL, yaxt.in = NULL, xaxt.out = NULL, xaxt.in = NULL, axis1.in = FALSE, at1.in = NULL, axis2.in = FALSE, at2.in = NULL, axis1.out = FALSE, at1.out = NULL, axis2.out = FALSE, at2.out = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
}
  \item{legendSide}{
%%     ~~Describe \code{legendSide} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{lty}{
%%     ~~Describe \code{lty} here~~
}
  \item{yaxt.out}{
%%     ~~Describe \code{yaxt.out} here~~
}
  \item{yaxt.in}{
%%     ~~Describe \code{yaxt.in} here~~
}
  \item{xaxt.out}{
%%     ~~Describe \code{xaxt.out} here~~
}
  \item{xaxt.in}{
%%     ~~Describe \code{xaxt.in} here~~
}
  \item{axis1.in}{
%%     ~~Describe \code{axis1.in} here~~
}
  \item{at1.in}{
%%     ~~Describe \code{at1.in} here~~
}
  \item{axis2.in}{
%%     ~~Describe \code{axis2.in} here~~
}
  \item{at2.in}{
%%     ~~Describe \code{at2.in} here~~
}
  \item{axis1.out}{
%%     ~~Describe \code{axis1.out} here~~
}
  \item{at1.out}{
%%     ~~Describe \code{at1.out} here~~
}
  \item{axis2.out}{
%%     ~~Describe \code{axis2.out} here~~
}
  \item{at2.out}{
%%     ~~Describe \code{at2.out} here~~
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
function (data, var = "co2_flux", legend = FALSE, legendSide = NULL, 
    type = "o", col = 1, lty = 1, yaxt.out = NULL, yaxt.in = NULL, 
    xaxt.out = NULL, xaxt.in = NULL, axis1.in = FALSE, at1.in = NULL, 
    axis2.in = FALSE, at2.in = NULL, axis1.out = FALSE, at1.out = NULL, 
    axis2.out = FALSE, at2.out = NULL, ...) 
{
    month <- c("January", "February", "March", "April", "May", 
        "June", "July", "August", "September", "October", "November", 
        "December")
    agg = aggregate(data[[var]], list(hour = data$hour, month = data$month), 
        mean, na.rm = TRUE)
    parRaw <- ifelse(length(unique(data$month)) == 12, 3, length(which(unique(data$month)\%\%3 == 
        0)))
    parCol <- ifelse(((length(unique(data$month)) - 4)\%\%4) == 
        0, 4, ifelse(length(unique(data$month)) < 4, length(unique(data$month)), 
        4))
    par(mfrow = c(parRaw, parCol), mar = c(0, 0.8, 0, 0), oma = c(5, 
        5, 5, 5))
    for (i in unique(data$month)[!is.na(unique(data$month))]) {
        if (i == 1 | i == 5 | i == 9) {
            plot(agg[which(agg$month == i), 3] ~ agg[which(agg$month == 
                i), 1], type = type, col = col, lty = lty, yaxt = yaxt.out, 
                xaxt = xaxt.out, ...)
            axis(1, at1.out, labels = axis1.out)
            axis(2, at2.out, labels = axis2.out)
            abline(h = 0, lty = 2)
        }
        else {
            plot(agg[which(agg$month == i), 3] ~ agg[which(agg$month == 
                i), 1], type = type, col = col, lty = lty, yaxt = yaxt.in, 
                xaxt = xaxt.in, ...)
            axis(1, at1.in, labels = axis1.in)
            axis(2, at2.in, labels = axis2.in)
            abline(h = 0, lty = 2)
        }
        if (legend) {
            if (!is.null(legendSide)) {
                legend(legendSide, month[i], lty = -1, pch = -1, 
                  bty = "n", cex = 1)
            }
            else if (is.null(legendSide)) {
                print("You must give which side to draw the legend", 
                  quote = FALSE)
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
