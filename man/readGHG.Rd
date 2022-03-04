\name{readGHG}
\alias{readGHG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{readGHG
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readGHG(file, TimeZone = "GMT")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{TimeZone}{
%%     ~~Describe \code{TimeZone} here~~
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
function (file, TimeZone = "GMT") 
{
    crTimeSt <- function(data, TimeZone = TimeZone, flux = TRUE) {
        if (flux) {
            data$timestamp <- paste(data$Date, data$Time, sep = " ")
        }
        else {
            data$timestamp <- paste(data$DATE, data$TIME, sep = " ")
        }
        data$timestamp <- as.POSIXct(data$timestamp, tz = TimeZone)
        data$year <- lubridate::year(data$timestamp)
        data$month <- lubridate::month(data$timestamp)
        data$yday <- lubridate::yday(data$timestamp)
        data$hour <- lubridate::hour(data$timestamp) + lubridate::minute(data$timestamp)/60
        data$minute <- lubridate::minute(data$timestamp)
        data$second <- lubridate::second(data$timestamp)
        return(data)
    }
    flx <- readr::read_delim(file = unz(file, paste0(tools::file_path_sans_ext(file), 
        ".data")), col_names = F, skip = 9, delim = "\t")
    n.flx <- readr::read_delim(file = unz(file, paste0(tools::file_path_sans_ext(file), 
        ".data")), col_names = T, skip = 7, delim = "\t", n_max = 1)
    bmt <- readr::read_delim(file = unz(file, paste0(tools::file_path_sans_ext(file), 
        "-biomet.data")), col_names = F, skip = 6, delim = "\t")
    n.bmt <- readr::read_delim(file = unz(file, paste0(tools::file_path_sans_ext(file), 
        "-biomet.data")), col_names = T, skip = 5, delim = "\t", 
        n_max = 1)
    names(flx) <- names(n.flx)
    names(bmt) <- names(n.bmt)
    flx <- crTimeSt(flx, TimeZone = TimeZone, flux = TRUE)
    bmt <- crTimeSt(bmt, TimeZone = TimeZone, flux = FALSE)
    return(list(flux = flx, biomet = bmt))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
