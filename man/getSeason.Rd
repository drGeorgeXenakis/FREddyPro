\name{getSeason}
\alias{getSeason}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{getSeason
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getSeason(input.date)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input.date}{
%%     ~~Describe \code{input.date} here~~
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
function (input.date) 
{
    require(lubridate)
    numeric.date <- 100 * month(input.date) + day(input.date)
    cuts <- base::cut(numeric.date, breaks = c(0, 319, 620, 921, 
        1220, 1231))
    levels(cuts) <- c("Winter", "Spring", "Summer", "Fall", "Winter")
    return(cuts)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
