\name{plotFingerprint}
\alias{plotFingerprint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plotFingerprint
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotFingerprint(var, doy, hour, step = 2, xlab = "Hour", ylab = "Day or Year", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{doy}{
%%     ~~Describe \code{doy} here~~
}
  \item{hour}{
%%     ~~Describe \code{hour} here~~
}
  \item{step}{
%%     ~~Describe \code{step} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
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
function (var, doy, hour, step = 2, xlab = "Hour", ylab = "Day or Year", 
    ...) 
{
    new.red = rgb(255/255, 0/255, 0/255)
    new.yellow = rgb(255/255, 255/255, 0/255)
    new.blue = rgb(0/255, 0/255, 255/255)
    rgb.palette <- colorRampPalette(c(new.blue, new.yellow, new.red))
    data <- data.frame(cbind(doy, hour, var))
    m <- data.matrix(reshape(data, v.names = "var", timevar = "doy", 
        idvar = "hour", direction = "wide"))
    doy.lim = seq(range(doy)[1], range(doy)[2], step)
    at.seq = seq(0, max(doy.lim)/range(doy)[2], length = length(doy.lim))
    seq(range(doy)[1], range(doy)[2], 12)
    filled.contour(m, color.palette = rgb.palette, ylab = "Day of Year", 
        xlab = "Hour", plot.axes = {
            axis(side = 2, at = at.seq, labels = as.character(doy.lim))
            axis(side = 1, at = seq(0, 1, 1/23), labels = as.character(seq(0, 
                23, 1)))
        }, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
