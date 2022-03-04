\name{ftpToDfPerc}
\alias{ftpToDfPerc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ftpToDfPerc
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ftpToDfPerc(ftp, percent)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ftp}{
%%     ~~Describe \code{ftp} here~~
}
  \item{percent}{
%%     ~~Describe \code{percent} here~~
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
function (ftp, percent) 
{
    x = ftp[[2]]
    y = ftp[[3]]
    z = ftp[[1]]
    df = data.frame(x = numeric(length(x)), y = numeric(length(y)), 
        z = numeric(length(cumsum(z))))
    df$z = as.vector(z)
    df$x = as.vector(x)
    df$y = as.vector(y)
    t <- df[order(-df$z), ]
    t$cumz <- cumsum(t$z) * 100
    t$cumz = ifelse(t$cumz <= percent, t$cumz, NA)
    return(t)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
