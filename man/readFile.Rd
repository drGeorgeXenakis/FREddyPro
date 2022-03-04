\name{readFile}
\alias{readFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{readFile
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readFile(dataFile, nSkip = 0, nSkipNames = 0, header = FALSE, reverse = FALSE, sep = ",", na = "NaN", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataFile}{
%%     ~~Describe \code{dataFile} here~~
}
  \item{nSkip}{
%%     ~~Describe \code{nSkip} here~~
}
  \item{nSkipNames}{
%%     ~~Describe \code{nSkipNames} here~~
}
  \item{header}{
%%     ~~Describe \code{header} here~~
}
  \item{reverse}{
%%     ~~Describe \code{reverse} here~~
}
  \item{sep}{
%%     ~~Describe \code{sep} here~~
}
  \item{na}{
%%     ~~Describe \code{na} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (dataFile, nSkip = 0, nSkipNames = 0, header = FALSE, 
    reverse = FALSE, sep = ",", na = "NaN", ...) 
{
    if (!reverse) {
        if (nSkip == 0) {
            df <- readr::read_delim(file = dataFile, col_names = TRUE, 
                delim = sep, na = na, ...)
        }
        else {
            df <- readr::read_delim(file = dataFile, col_names = FALSE, 
                skip = nSkip, delim = sep, na = na, ...)
            if (nSkipNames != 0) {
                n <- readr::read_delim(file = dataFile, col_names = TRUE, 
                  skip = nSkipNames, n_max = 1, delim = sep, 
                  na = na, ...)
                names(df) <- names(n)
            }
        }
    }
    else {
        df <- readr::read_delim(file = dataFile, col_names = F, 
            skip = nSkip, delim = sep, na = na, ...)
        n <- readr::read_delim(file = dataFile, col_names = T, 
            skip = nSkipNames, n_max = 1, na = na, delim = sep, 
            ...)
        names(df) <- names(n)
    }
    return(df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
