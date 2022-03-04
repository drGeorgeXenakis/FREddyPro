\name{createTimestamp}
\alias{createTimestamp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{createTimestamp
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
createTimestamp(data, timestamp = NULL, timeZone = "GMT")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{timestamp}{
%%     ~~Describe \code{timestamp} here~~
}
  \item{timeZone}{
%%     ~~Describe \code{timeZone} here~~
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
function (data, timestamp = NULL, timeZone = "GMT") 
{
    if (is.null(timestamp)) {
        data$timestamp <- paste(data$date, data$time, sep = " ")
        data$timestamp <- as.POSIXct(data$timestamp, tz = timeZone)
        data$year <- year(data$timestamp)
        data$month <- month(data$timestamp)
        data$yday <- yday(data$timestamp)
        data$hour <- hour(data$timestamp) + minute(data$timestamp)/60
        data$minute <- minute(data$timestamp)
        data$second <- second(data$timestamp)
    }
    else {
        data[[timestamp]] <- as.POSIXct(data[[timestamp]], tz = timeZone)
        data$year <- year(data[[timestamp]])
        data$month <- month(data[[timestamp]])
        data$yday <- yday(data[[timestamp]])
        data$hour <- hour(data[[timestamp]]) + minute(data[[timestamp]])/60
        data$minute <- minute(data[[timestamp]])
        data$second <- second(data[[timestamp]])
    }
    return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
