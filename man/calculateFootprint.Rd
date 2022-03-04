\name{calculateFootprint}
\alias{calculateFootprint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{calculateFootprint
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calculateFootprint(df, displacement, stability = NULL, fetch = 500, grid = 200, height, lowerDay = NULL, upperDay = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{displacement}{
%%     ~~Describe \code{displacement} here~~
}
  \item{stability}{
%%     ~~Describe \code{stability} here~~
}
  \item{fetch}{
%%     ~~Describe \code{fetch} here~~
}
  \item{grid}{
%%     ~~Describe \code{grid} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
}
  \item{lowerDay}{
%%     ~~Describe \code{lowerDay} here~~
}
  \item{upperDay}{
%%     ~~Describe \code{upperDay} here~~
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
function (df, displacement, stability = NULL, fetch = 500, grid = 200, 
    height, lowerDay = NULL, upperDay = NULL) 
{
    df <- df[which(!is.na(df$L)), ]
    if (is.null(lowerDay) & !is.null(upperDay)) {
        df <- df[which(df$DOY <= upperDay), ]
    }
    else if (!is.null(lowerDay) & is.null(upperDay)) {
        print("In not null")
        df <- df[which(df$DOY >= lowerDay), ]
    }
    else if (!is.null(lowerDay) & !is.null(upperDay)) {
        df <- df[which(df$DOY >= lowerDay & df$DOY <= upperDay), 
            ]
    }
    df$z = (df$X.z.d..L * df$L) + displacement
    df$zol = df$z/df$L
    if (is.null(stability)) {
        stab = seq(1, nrow(df), 1)
    }
    else if (stability == 1) {
        stab = which(df$zol < -0.0625)
    }
    else if (stability == 2) {
        stab = which(df$zol >= -0.0625 & df$zol <= 0.0625)
    }
    else if (stability == 3) {
        stab = which(df$zol > 0.0625)
    }
    df <- df[stab, ]
    footprint = Average(fetch = fetch, height = height, grid = grid, 
        df$wind_speed, df$wind_dir, df$u., df$zol, sqrt(df$v_var))
    return(footprint)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
