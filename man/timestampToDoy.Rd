\name{timestampToDoy}
\alias{timestampToDoy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{timestampToDoy
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
timestampToDoy(ts, decimals = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
%%     ~~Describe \code{ts} here~~
}
  \item{decimals}{
%%     ~~Describe \code{decimals} here~~
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
function (ts, decimals = 3) 
{
    doy <- round(as.numeric(strftime(as.POSIXlt(ts), format = "\%j")) + 
        as.numeric(strftime(as.POSIXlt(ts), format = "\%H"))/24 + 
        as.numeric(strftime(as.POSIXlt(ts), format = "\%M"))/(24 * 
            60) + as.numeric(strftime(as.POSIXlt(ts), format = "\%S"))/(24 * 
        60 * 60), decimals)
    return(doy)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
