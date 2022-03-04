\name{fillTimestamp}
\alias{fillTimestamp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{fillTimestamp
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fillTimestamp(data, timestamp = "timestamp", tz = "GMT", timediff = 30, units = "min")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{timestamp}{
%%     ~~Describe \code{timestamp} here~~
}
  \item{tz}{
%%     ~~Describe \code{tz} here~~
}
  \item{timediff}{
%%     ~~Describe \code{timediff} here~~
}
  \item{units}{
%%     ~~Describe \code{units} here~~
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
function (data, timestamp = "timestamp", tz = "GMT", timediff = 30, 
    units = "min") 
{
    data <- data.frame(data)
    if ("time" \%in\% names(data)) 
        data[["time"]] <- as.character(data[["time"]])
    if (timestamp \%in\% names(data)) {
        ct <- continuity(data, timestamp = timestamp, timediff = timediff, 
            units = units, return = T)
        if (nrow(ct) == 0) {
            cat("No discontinuity")
        }
        else {
            for (i in 1:nrow(ct)) {
                rN <- which(data[[timestamp]] == ct[[1]][i])
                timeStart <- as.POSIXct(data[rN, timestamp], 
                  tz = tz)
                timeEnd <- as.POSIXct(data[rN + 1, timestamp], 
                  tz = tz) - minutes(30)
                timeDif <- difftime(timeEnd, timeStart, units = "mins")
                print(timeDif)
                numberHalfHours = timeDif[[1]]/30
                emptyRows <- createEmptyRows(data, ts = timestamp, 
                  nHH = numberHalfHours, tS = timeStart, tE = timeEnd)
                data <- rbind.data.frame(data[1:(rN), ], emptyRows, 
                  data[(rN + 1):nrow(data), ])
            }
        }
    }
    else {
        cat("Please give a timestamp")
    }
    if ("time" \%in\% names(data)) 
        data[["time"]] <- hms::as_hms(data[["time"]])
    data <- as_tibble(data)
    return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
