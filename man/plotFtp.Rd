\name{plotFtp}
\alias{plotFtp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plotFtp
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotFtp(ftp, tl = NULL, xy = 500, j = 200)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ftp}{
%%     ~~Describe \code{ftp} here~~
}
  \item{tl}{
%%     ~~Describe \code{tl} here~~
}
  \item{xy}{
%%     ~~Describe \code{xy} here~~
}
  \item{j}{
%%     ~~Describe \code{j} here~~
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
function (ftp, tl = NULL, xy = 500, j = 200) 
{
    l <- ggplot(ftp, aes(x, y))
    l <- l + geom_raster(aes(fill = cumz), interpolate = T) + 
        scale_fill_gradientn(colours = rainbow(7), name = "Cumulative\nfootprint [\%]", 
            limits = c(0, 100), breaks = seq(0, 100, 20), na.value = "transparent") + 
        theme_bw() + scale_x_continuous(limits = c(-xy, xy), 
        breaks = seq(-xy, xy, j)) + scale_y_continuous(limits = c(-xy, 
        xy), breaks = seq(-xy, xy, j)) + labs(x = "Distance from tower [m]", 
        y = "Distance from tower [m]", title = tl) + theme(panel.background = element_blank(), 
        plot.title = element_text(colour = "black", size = 12, 
            hjust = 0.5), axis.title.x = element_text(colour = "black", 
            size = 10), axis.title.y = element_text(colour = "black", 
            size = 10), axis.text.x = element_text(colour = "black", 
            size = 10), axis.text.y = element_text(colour = "black", 
            size = 10), axis.ticks.x = element_line(colour = "black"), 
        axis.ticks.y = element_line(colour = "black"), panel.grid.major.x = element_line(colour = "grey"), 
        panel.grid.major.y = element_line(colour = "grey"))
    return(l)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
