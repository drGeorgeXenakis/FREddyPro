\name{sdClean}
\alias{sdClean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{sdClean
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sdClean(var, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
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
function (var, p) 
{
    positive <- which(var > 0)
    negative <- which(var < 0)
    m.positive <- mean(var[positive], na.rm = TRUE)
    m.negative <- mean(var[negative], na.rm = TRUE)
    sd.positive <- sd(var[positive], na.rm = TRUE)
    sd.negative <- sd(var[negative], na.rm = TRUE)
    filter.positive <- m.positive + p * sd.positive
    filter.negative <- m.negative - p * sd.negative
    rm.positive <- which(var > filter.positive)
    rm.negative <- which(var < filter.negative)
    var[rm.positive] <- NA
    var[rm.negative] <- NA
    return(var)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
