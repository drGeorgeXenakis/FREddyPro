\name{Average}
\alias{Average}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Average
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Average(fetch = 500, height = 3, grid = 200, speed, direction, uStar, zol, sigmaV, weights = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fetch}{
%%     ~~Describe \code{fetch} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
}
  \item{grid}{
%%     ~~Describe \code{grid} here~~
}
  \item{speed}{
%%     ~~Describe \code{speed} here~~
}
  \item{direction}{
%%     ~~Describe \code{direction} here~~
}
  \item{uStar}{
%%     ~~Describe \code{uStar} here~~
}
  \item{zol}{
%%     ~~Describe \code{zol} here~~
}
  \item{sigmaV}{
%%     ~~Describe \code{sigmaV} here~~
}
  \item{weights}{
%%     ~~Describe \code{weights} here~~
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
function (fetch = 500, height = 3, grid = 200, speed, direction, 
    uStar, zol, sigmaV, weights = NULL) 
{
    grid = 2 * floor(grid/2)
    lens = c(length(speed), length(direction), length(uStar), 
        length(zol), length(sigmaV))
    if (!is.null(weights)) {
        lens = c(lens, length(weights))
    }
    if (min(lens) == max(lens)) {
        ids = !is.na(speed)
        idd = !is.na(direction)
        idu = !is.na(uStar)
        idz = !is.na(zol)
        idv = !is.na(sigmaV)
    }
    idok = ids & idd & idu & idz & idv
    if (!is.null(weights)) {
        idw = !is.na(weights)
        idok = idok & idw
    }
    speed = speed[idok]
    direction = direction[idok]
    uStar = uStar[idok]
    zol = zol[idok]
    sigmaV = sigmaV[idok]
    if (!is.null(weights)) {
        weights = weights[idok]
    }
    if (length(speed) >= 1) {
        avg_footprint = list(Probability = matrix(0, nrow = grid, 
            ncol = grid))
        if (!is.null(weights)) {
            avg_footprint = list(Probability = matrix(0, nrow = grid, 
                ncol = grid), WeightedProbability = matrix(0, 
                nrow = grid, ncol = grid))
        }
        finput = t(mapply(list, speed, direction, uStar, zol, 
            sigmaV))
        icnt = 0
        for (i in 1:nrow(finput)) {
            ftp <- Calculate(fetch, height, grid, finput[[i, 
                1]], finput[[i, 2]], finput[[i, 3]], finput[[i, 
                4]], finput[[i, 5]])
            if (any(!is.na(ftp$footprint))) {
                avg_footprint$Probability = avg_footprint$Probability + 
                  ftp$footprint
                if (!is.null(weights)) {
                  avg_footprint$WeightedProbability = avg_footprint$WeightedProbability + 
                    ftp$footprint * weights[i]
                }
            }
            icnt = icnt + 1
        }
    }
    avg_footprint$Probability = avg_footprint$Probability/icnt
    returnList = list(Probability = avg_footprint$Probability, 
        FPe = ftp$FPe, FPn = ftp$FPn, WeightedProbability = avg_footprint$WeightedProbability)
    return(returnList)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
