\name{qcClean}
\alias{qcClean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{qcClean
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
qcClean(var, qcVar, qc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{qcVar}{
%%     ~~Describe \code{qcVar} here~~
}
  \item{qc}{
%%     ~~Describe \code{qc} here~~
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
function (var, qcVar, qc) 
{
    if (length(qc) != 1) {
        for (i in 1:length(qc)) {
            qc.index <- which(qcVar == qc[i])
            var[qc.index] <- NA
        }
    }
    else {
        qc.index <- which(qcVar == qc)
        var[qc.index] <- NA
    }
    return(var)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
