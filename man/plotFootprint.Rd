\name{plotFootprint}
\alias{plotFootprint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plotFootprint
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotFootprint(footprint, main.title = NULL, key.title = NULL, color.palette = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{footprint}{
%%     ~~Describe \code{footprint} here~~
}
  \item{main.title}{
%%     ~~Describe \code{main.title} here~~
}
  \item{key.title}{
%%     ~~Describe \code{key.title} here~~
}
  \item{color.palette}{
%%     ~~Describe \code{color.palette} here~~
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
function (footprint, main.title = NULL, key.title = NULL, color.palette = NULL, 
    ...) 
{
    new.red = rgb(255/255, 0/255, 0/255)
    new.yellow = rgb(255/255, 255/255, 0/255)
    new.blue = rgb(0/255, 0/255, 255/255)
    if (is.null(color.palette)) {
        rgb.palette <- colorRampPalette(c(new.blue, new.yellow, 
            new.red))
        color.palette = rgb.palette
    }
    ftpr = t(apply(footprint[[1]], 2, rev))
    cord = unique(as.vector(footprint[[2]]))
    filled.contour(cord, cord, ftpr, color.palette = color.palette, 
        plot.title = title(main = main.title), key.title = title(main = key.title, 
            adj = 0.2), ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
