\name{writeDateTime}
\alias{writeDateTime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{writeDateTime
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
writeDateTime(df, timestamp = "timestamp", date = "date", time = "time", doy = "DOY")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{timestamp}{
%%     ~~Describe \code{timestamp} here~~
}
  \item{date}{
%%     ~~Describe \code{date} here~~
}
  \item{time}{
%%     ~~Describe \code{time} here~~
}
  \item{doy}{
%%     ~~Describe \code{doy} here~~
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
function (df, timestamp = "timestamp", date = "date", time = "time", 
    doy = "DOY") 
{
    if (is.null(df[[timestamp]])) {
        stop("Timestamp does not exist")
    }
    else {
        if (any(is.na(df[[date]]))) {
            nR <- which(is.na(df[[date]]))
            if (date \%in\% names(df)) 
                df[nR, date] <- as.factor(as.Date(df[nR, timestamp]))
            if (time \%in\% names(df)) 
                df[nR, time] <- format(df[nR, timestamp], format = "\%H:\%M")
            if (doy \%in\% names(df)) 
                df[nR, doy] <- doy(df[nR, timestamp])
        }
    }
    return(df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
