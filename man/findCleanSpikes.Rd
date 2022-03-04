\name{findCleanSpikes}
\alias{findCleanSpikes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{findCleanSpikes
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
findCleanSpikes(data, x, qc = NULL, qcFlag = NULL, times = 3, quantiles = c(0.05, 0.95), group = hour, plotSpikes = FALSE, removeSpikes = FALSE, returnDataFrameOnly = FALSE, limits = c(-50, 30), breaks = seq(-50, 30, 20))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{qc}{
%%     ~~Describe \code{qc} here~~
}
  \item{qcFlag}{
%%     ~~Describe \code{qcFlag} here~~
}
  \item{times}{
%%     ~~Describe \code{times} here~~
}
  \item{quantiles}{
%%     ~~Describe \code{quantiles} here~~
}
  \item{group}{
%%     ~~Describe \code{group} here~~
}
  \item{plotSpikes}{
%%     ~~Describe \code{plotSpikes} here~~
}
  \item{removeSpikes}{
%%     ~~Describe \code{removeSpikes} here~~
}
  \item{returnDataFrameOnly}{
%%     ~~Describe \code{returnDataFrameOnly} here~~
}
  \item{limits}{
%%     ~~Describe \code{limits} here~~
}
  \item{breaks}{
%%     ~~Describe \code{breaks} here~~
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
function (data, x, qc = NULL, qcFlag = NULL, times = 3, quantiles = c(0.05, 
    0.95), group = hour, plotSpikes = FALSE, removeSpikes = FALSE, 
    returnDataFrameOnly = FALSE, limits = c(-50, 30), breaks = seq(-50, 
        30, 20)) 
{
    require(lazyeval)
    require(tidyverse)
    data <- data \%>\% select(-one_of("spikes", "fn1", "fn2"))
    outList <- list()
    v <- enquo(x)
    qc_v <- enquo(qc)
    grp <- enquo(group)
    if (missing(qc)) {
        dataSpk <- data
    }
    else {
        if (is.null(qcFlag)) {
            dataSpk <- data
        }
        else {
            dataSpk <- data \%>\% mutate(`:=`(!!v, if_else(!!qc_v != 
                qcFlag, !!v, NA_real_)), `:=`(!!qc_v, if_else(!!qc_v != 
                qcFlag, !!qc_v, NA_real_)), )
        }
    }
    p_funs <- map(quantiles, ~partial(quantile, probs = .x, na.rm = TRUE))
    quantDF <- dataSpk \%>\% group_by(!!grp) \%>\% summarise_at(vars(!!v), 
        .funs = c(p_funs))
    mean.positive <- dataSpk \%>\% filter(!!v > 0) \%>\% select(!!v) \%>\% 
        summarise(mean = mean(!!v, na.rm = T)) \%>\% as.numeric()
    mean.negative <- dataSpk \%>\% filter(!!v < 0) \%>\% select(!!v) \%>\% 
        summarise(mean = mean(!!v, na.rm = T)) \%>\% as.numeric()
    sd.positive <- dataSpk \%>\% filter(!!v > 0) \%>\% select(!!v) \%>\% 
        summarise(sd = sd(!!v, na.rm = T)) \%>\% as.numeric()
    sd.negative <- dataSpk \%>\% filter(!!v < 0) \%>\% select(!!v) \%>\% 
        summarise(sd = sd(!!v, na.rm = T)) \%>\% as.numeric()
    dataSpk <- dataSpk \%>\% mutate(spikes = if_else(!!v > 0, if_else(!!v > 
        (mean.positive + times * sd.positive), 1, 0), if_else(!!v < 
        (mean.negative - times * sd.negative), 1, 0))) \%>\% left_join(quantDF, 
        by = quo_name(grp)) \%>\% mutate(spikes = if_else(!!v > 
        fn2, spikes + 1, if_else(!!v < fn1, spikes + 1, 0)))
    if (plotSpikes) {
        if (missing(qc)) {
            pDiurnal <- ggplot(data = dataSpk) + geom_point(aes(x = !!grp, 
                y = !!v, colour = as.factor(spikes)), shape = 16) + 
                geom_line(aes(x = !!grp, y = fn1)) + geom_line(aes(x = !!grp, 
                y = fn2), colour = "blue") + theme_bw() + scale_y_continuous(limits = limits, 
                breaks = breaks) + theme(panel.grid.major = element_line(colour = "grey70"), 
                panel.grid.minor = element_line(colour = "grey70"))
            pTimeseries <- ggplot(data = dataSpk) + geom_point(aes(x = timestamp, 
                y = !!v, colour = as.factor(spikes)), shape = 16) + 
                theme_bw() + scale_y_continuous(limits = limits, 
                breaks = breaks) + theme(panel.grid.major = element_line(colour = "grey70"), 
                panel.grid.minor = element_line(colour = "grey70"))
            outList[[2]] <- pDiurnal
            outList[[3]] <- pTimeseries
        }
        else {
            pDiurnal <- ggplot(data = dataSpk) + geom_point(aes(x = !!grp, 
                y = !!v, colour = as.factor(spikes), shape = as.factor(!!qc_v))) + 
                geom_line(aes(x = !!grp, y = fn1)) + geom_line(aes(x = !!grp, 
                y = fn2), colour = "blue") + theme_bw() + scale_y_continuous(limits = limits, 
                breaks = breaks) + theme(panel.grid.major = element_line(colour = "grey70"), 
                panel.grid.minor = element_line(colour = "grey70"))
            pTimeseries <- ggplot(data = dataSpk) + geom_point(aes(x = timestamp, 
                y = !!v, colour = as.factor(spikes), shape = as.factor(!!qc_v))) + 
                theme_bw() + scale_y_continuous(limits = limits, 
                breaks = breaks) + theme(panel.grid.major = element_line(colour = "grey70"), 
                panel.grid.minor = element_line(colour = "grey70"))
            outList[[2]] <- pDiurnal
            outList[[3]] <- pTimeseries
        }
    }
    if (removeSpikes) {
        dataDspk <- dataSpk \%>\% mutate(`:=`(!!v, if_else(spikes == 
            0, !!v, NA_real_)))
        if (!missing(qc)) 
            dataDspk <- dataDspk \%>\% mutate(`:=`(!!qc_v, if_else(spikes == 
                0, !!qc_v, NA_real_)))
        outList[[1]] <- dataDspk
    }
    else {
        outList[[1]] <- dataSpk
    }
    if (returnDataFrameOnly) {
        return(as_tibble(outList[[1]]))
    }
    else {
        return(outList)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
