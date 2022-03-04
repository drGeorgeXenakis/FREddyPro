\name{buildTimestamp}
\alias{buildTimestamp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{buildTimestamp
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
buildTimestamp(data, doy, decimal.hour, origin = "2016-01-01")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{doy}{
%%     ~~Describe \code{doy} here~~
}
  \item{decimal.hour}{
%%     ~~Describe \code{decimal.hour} here~~
}
  \item{origin}{
%%     ~~Describe \code{origin} here~~
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
function (data, doy, decimal.hour, origin = "2016-01-01") 
{
    dayOfYear = data[[doy]]
    d.hour = data[[decimal.hour]]
    hour = floor(d.hour)
    minutes = (d.hour - hour) * 60
    seconds = rep(0, length(hour))
    timestamp = paste(as.Date(dayOfYear - 1, origin = origin), 
        " ", sprintf("\%02d", hour), ":", sprintf("\%02d", minutes), 
        ":", sprintf("\%02d", seconds), sep = "")
    spl = cbind(as.POSIXlt(timestamp), as.data.frame(data))
    names(spl) <- c("timestamp", names(as.data.frame(data)))
    return(spl)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
