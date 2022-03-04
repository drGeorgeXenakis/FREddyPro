\name{Calculate}
\alias{Calculate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Calculate(fetch = 500, height = 3, grid = 200, speed, direction, uStar, zol, sigmaV)
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
    uStar, zol, sigmaV) 
{
    grid = 2 * floor(grid/2)
    sigmaY = matrix(0, nrow = grid, ncol = grid)
    twopisigma = matrix(0, nrow = grid, ncol = grid)
    DispY = matrix(0, nrow = grid, ncol = grid)
    Pfp = matrix(0, nrow = grid, ncol = grid)
    Pf = matrix(0, nrow = grid, ncol = grid)
    auPlume = matrix(1, nrow = grid, ncol = grid)
    linn = seq(99.5, -99.5, length = grid)
    line = seq(-99.5, 99.5, length = grid)
    linn = linn * 2 * fetch/199
    line = line * 2 * fetch/199
    m = length(line)
    n = length(linn)
    FPe = matrix(rep(line, each = n), nrow = n)
    FPn = matrix(rep(linn, m), nrow = n)
    FPd = sqrt(FPn^2 + FPe^2)
    FPa = atan2(FPe, FPn) * 180/pi
    FPa = FPa - direction
    FPx = cos(FPa/57) * FPd
    FPy = sin(FPa/57) * FPd
    zt = ifelse(zol > 0, 0, (1 - 16 * zol)^0.25)
    phim = ifelse(zol > 0, 1 + 5 * zol, (1 - 16 * zol)^-0.25)
    phic = ifelse(zol > 0, 1 + 5 * zol, (1 - 16 * zol)^-0.5)
    psim = ifelse(zol > 0, 5 * zol, -2 * log((1 + zt)/2) - log((1 + 
        zt * zt)/2) + 2 * atan(zt) - pi/2)
    nn = ifelse(zol > 0, 1/phic, (1 - 24 * zol)/(1 - 16 * zol))
    eddydif = 0.41 * uStar * height/phic
    mm = uStar * phim/(0.41 * speed)
    rr = 2 + mm - nn
    mu = (1 + mm)/rr
    alpu = speed/(height^mm)
    alpk = eddydif/(height^nn)
    xi = alpu * height^rr/(((rr)^2) * alpk)
    fgamma = gamma(mu)
    idup = FPx > 0
    Pf[idup] = (1/fgamma) * (xi^mu)/(FPx[idup]^(1 + mu)) * exp(-1 * 
        xi/FPx[idup])
    uPlume = (gamma(mu)/gamma(1/rr))
    uPlume = uPlume * ((rr * rr * 0.4/speed)^(mm/rr))
    auPlume[idup] = uPlume * (speed * (FPx[idup]^(mm/rr)))
    sigmaY[idup] = sigmaV * FPx[idup]/auPlume[idup]
    twopisigma[idup] = 1/((sqrt(2 * pi) * sigmaY[idup]))
    DispY[idup] = twopisigma[idup] * exp(-1 * FPy[idup] * FPy[idup]/(2 * 
        sigmaY[idup] * sigmaY[idup]))
    Pfp[idup] = Pf[idup] * DispY[idup]
    footprint = Pfp/sum(sum(Pfp))
    returnList = list(footprint = footprint, FPe = FPe, FPn = FPn)
    return(returnList)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
