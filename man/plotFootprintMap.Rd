\name{plotFootprintMap}
\alias{plotFootprintMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plotFootprintMap
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotFootprintMap(footprint, X, Y, zoom, maptype = "hybrid", alpha = 0.35)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{footprint}{
%%     ~~Describe \code{footprint} here~~
}
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{zoom}{
%%     ~~Describe \code{zoom} here~~
}
  \item{maptype}{
%%     ~~Describe \code{maptype} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
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
function (footprint, X, Y, zoom, maptype = "hybrid", alpha = 0.35) 
{
    x = t(apply(footprint[[2]], 2, rev))
    y = t(apply(footprint[[3]], 2, rev))
    z = t(apply(footprint[[1]], 2, rev))
    df = data.frame(x = numeric(length(as.vector(x))), y = numeric(length(as.vector(y))), 
        z = numeric(length(as.vector(z))))
    df$x = as.vector(x)
    df$y = as.vector(y)
    df$z = as.vector(z)
    dfosgb36 <- df
    dfosgb36$x <- dfosgb36[, 1] + X
    dfosgb36$y <- dfosgb36[, 2] + Y
    r = rasterFromXYZ(dfosgb36)
    crs(r) <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
    micolor <- rev(rainbow(12, alpha = alpha))
    transp <- rainbow(12, alpha = 0)
    micolor[1:1] <- transp[1]
    mymap <- gmap(x = r, type = maptype, zoom = zoom)
    rGM <- projectRaster(from = r, crs = CRS("+init=epsg:3857"))
    rGM[rGM < 0] <- 0
    rGM[rGM > 1] <- 1
    plot(mymap)
    plot(rGM, add = T, legend = F, col = micolor)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
