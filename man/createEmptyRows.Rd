\name{createEmptyRows}
\alias{createEmptyRows}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{createEmptyRows
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
createEmptyRows(df, ts = timestamp, nHH, tS, tE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{ts}{
%%     ~~Describe \code{ts} here~~
}
  \item{nHH}{
%%     ~~Describe \code{nHH} here~~
}
  \item{tS}{
%%     ~~Describe \code{tS} here~~
}
  \item{tE}{
%%     ~~Describe \code{tE} here~~
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
function (df, ts = timestamp, nHH, tS, tE) 
{
    if (nHH == 0) {
        tst <- tE
    }
    else {
        tst <- seq.POSIXt(tS, tE, "30 min")
    }
    f <- function(x) NA
    nR <- df \%>\% mutate_all(f) \%>\% slice(1:length(tst))
    nR[[ts]] <- tst
    if ("date" \%in\% names(df)) 
        nR[["date"]] <- as.Date(nR[[ts]])
    if ("time" \%in\% names(df)) 
        nR[["time"]] <- hms::as_hms(ymd_hms(nR[[ts]]))
    if ("DOY" \%in\% names(df)) 
        nR[["DOY"]] <- timestampToDoy(nR[[ts]])
    return(nR)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
