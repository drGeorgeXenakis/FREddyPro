\name{distClean}
\alias{distClean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{distClean
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
distClean(var, hour, df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{hour}{
%%     ~~Describe \code{hour} here~~
}
  \item{df}{
%%     ~~Describe \code{df} here~~
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
function (var, hour, df) 
{
    m <- aggregate(var ~ hour, data = df, mean)
    q <- as.data.frame(t(as.data.frame(t(aggregate(var ~ hour, 
        data = df, quantile, prob = c(0.05, 0.95))))))
    p <- merge(m, q, "hour")
    names(p)[c(3, 4)] <- c("quant5", "quant95")
    for (i in 1:nrow(p)) {
        var[which(hour == p$hour[i])] <- ifelse(var[which(hour == 
            p$hour[i])] < p$quant5[i], NA, var[which(hour == 
            p$hour[i])])
        var[which(hour == p$hour[i])] <- ifelse(var[which(hour == 
            p$hour[i])] > p$quant95[i], NA, var[which(hour == 
            p$hour[i])])
    }
    return(var)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
