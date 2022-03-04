\name{continuity}
\alias{continuity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{continuity
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
continuity(data, timestamp = NULL, timediff = 30, units = "min", return = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{timestamp}{
%%     ~~Describe \code{timestamp} here~~
}
  \item{timediff}{
%%     ~~Describe \code{timediff} here~~
}
  \item{units}{
%%     ~~Describe \code{units} here~~
}
  \item{return}{
%%     ~~Describe \code{return} here~~
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
function (data, timestamp = NULL, timediff = 30, units = "min", 
    return = FALSE) 
{
    if (is.null(timestamp)) {
        data <- data \%>\% createTimestamp(data = .)
    }
    else {
        data <- data \%>\% createTimestamp(data = ., timestamp = timestamp)
    }
    data <- data \%>\% mutate(dt = c(difftime(tail(timestamp, -1), 
        head(timestamp, -1), units = units), 0))
    nDiscontinuity <- data \%>\% filter(dt > timediff) \%>\% select(timestamp) \%>\% 
        nrow
    if (nDiscontinuity != 0) {
        print(paste("Discontinuity in ", nDiscontinuity, " instances", 
            sep = ""), quote = F)
        discont <- data \%>\% filter(dt > timediff) \%>\% select(timestamp)
        discont \%>\% print
    }
    else {
        print("No discontinuity", quote = F)
    }
    if (return) 
        return(discont)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
