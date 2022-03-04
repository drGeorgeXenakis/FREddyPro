\name{cleanFluxes}
\alias{cleanFluxes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{cleanFluxes
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cleanFluxes(data, gas = "co2_flux", qcFlag = 2, sdCor = FALSE, sdTimes = 1, distCor = FALSE, agcCor = FALSE, agcVal = NULL, ustar = NULL, plot = FALSE, write = FALSE, outputFile, thresholdList = list(H = NULL, LE = NULL, Tau = NULL, h2o = NULL), timesList = list(H = NULL, LE = NULL, Tau = NULL, h2o = NULL), sunset = 19, sunrise = 6, na.value = "NaN")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{gas}{
%%     ~~Describe \code{gas} here~~
}
  \item{qcFlag}{
%%     ~~Describe \code{qcFlag} here~~
}
  \item{sdCor}{
%%     ~~Describe \code{sdCor} here~~
}
  \item{sdTimes}{
%%     ~~Describe \code{sdTimes} here~~
}
  \item{distCor}{
%%     ~~Describe \code{distCor} here~~
}
  \item{agcCor}{
%%     ~~Describe \code{agcCor} here~~
}
  \item{agcVal}{
%%     ~~Describe \code{agcVal} here~~
}
  \item{ustar}{
%%     ~~Describe \code{ustar} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
}
  \item{write}{
%%     ~~Describe \code{write} here~~
}
  \item{outputFile}{
%%     ~~Describe \code{outputFile} here~~
}
  \item{thresholdList}{
%%     ~~Describe \code{thresholdList} here~~
}
  \item{timesList}{
%%     ~~Describe \code{timesList} here~~
}
  \item{sunset}{
%%     ~~Describe \code{sunset} here~~
}
  \item{sunrise}{
%%     ~~Describe \code{sunrise} here~~
}
  \item{na.value}{
%%     ~~Describe \code{na.value} here~~
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
function (data, gas = "co2_flux", qcFlag = 2, sdCor = FALSE, 
    sdTimes = 1, distCor = FALSE, agcCor = FALSE, agcVal = NULL, 
    ustar = NULL, plot = FALSE, write = FALSE, outputFile, thresholdList = list(H = NULL, 
        LE = NULL, Tau = NULL, h2o = NULL), timesList = list(H = NULL, 
        LE = NULL, Tau = NULL, h2o = NULL), sunset = 19, sunrise = 6, 
    na.value = "NaN") 
{
    if (!is.data.frame(data)) {
        site <- readEddyPro(data, na = na.value)
    }
    else {
        site <- data
    }
    if (!all(c("year", "yday", "hour", "month") \%in\% names(site))) 
        site <- createTimestamp(site)
    if (plot) {
        if (!write) {
            if (dev.cur() == 1) {
                dev.new()
                dev.new()
            }
        }
    }
    if (length(timesList) == 1) {
        timesList = list(H = timesList, LE = timesList, Tau = timesList, 
            h2o = timesList)
    }
    if (plot) {
        if (write) {
            plots.before <- paste(dirname(outputFile), "/before.jpg", 
                sep = "")
            jpeg(plots.before, width = 1200, height = 800, res = 100)
        }
        else {
            dev.set(2)
        }
        par(mfrow = c(4, 3), oma = c(0, 0, 1.7, 0), mar = c(4, 
            4, 2, 2))
        plot(co2_flux ~ DOY, data = site, type = "p", pch = 16, 
            cex = 0.8)
        plot(H ~ DOY, data = site, col = 1, type = "p", pch = 16, 
            cex = 0.8)
        plot(LE ~ DOY, data = site, col = 1, type = "p", pch = 16, 
            cex = 0.8)
        plot(Tau ~ DOY, data = site, col = 1, type = "p", pch = 16, 
            cex = 0.8)
        plot(h2o_flux ~ DOY, data = site, col = 1, type = "p", 
            pch = 16, cex = 0.8)
        plot(ET ~ DOY, data = site, col = 1, type = "p", pch = 16, 
            cex = 0.8)
        plot(wind_speed ~ DOY, data = site, col = 1, type = "p", 
            pch = 16, cex = 0.8)
        plot(wind_dir ~ DOY, data = site, col = 1, type = "p", 
            pch = 16, cex = 0.8)
        plot(u. ~ DOY, data = site, col = 1, type = "p", pch = 16, 
            cex = 0.8)
        plot(air_temperature ~ DOY, data = site, col = 1, type = "p", 
            pch = 16, cex = 0.8)
        plot(VPD ~ DOY, data = site, col = 1, type = "p", pch = 16, 
            cex = 0.8)
        mtext("Before cleaning", outer = TRUE, cex = 1)
        if (write) {
            dev.off(2)
        }
    }
    gas_qc = paste("qc_", gas, sep = "")
    site[[gas]] <- qcClean(site[[gas]], site[[gas_qc]], qcFlag)
    if (sdCor) {
        site[[gas]] <- sdClean(site[[gas]], sdTimes)
    }
    if (distCor) {
        positive <- which(site[[gas]] > 0)
        negative <- which(site[[gas]] < 0)
        site[[gas]][positive] <- distClean(site[[gas]][positive], 
            site$hour[positive], site[positive, ])
        site[[gas]][negative] <- distClean(site[[gas]][negative], 
            site$hour[negative], site[negative, ])
    }
    if (!is.null(ustar)) {
        if (is.numeric(ustar)) {
            clean.ustar <- which(site$u. <= ustar)
            site[[gas]][clean.ustar] <- NA
        }
        else if (ustar == TRUE) {
            ust <- ustarThreshold(site, sunset = sunset, sunrise = sunrise)
            print(paste("Ustar threshold: ", round(ust, 3), sep = ""), 
                quote = FALSE)
            clean.ustar <- which(site$u. <= ust)
            site[[gas]][clean.ustar] <- NA
        }
        else if (ustar == FALSE) {
            print("No ustar filtering", quote = FALSE)
        }
    }
    if (agcCor) {
        clean.agc <- which(site[[agcVal]] < 50 | site[[agcVal]] > 
            60)
        site$co2_flux[clean.agc] <- NA
        site <- cleanSecondVar(x = "co2_flux", y = agcVal, data = site)
    }
    names_with_co2 = grep("co2", names(site))
    for (i in 2:length(names_with_co2)) {
        site <- cleanSecondVar(x = "co2_flux", y = names(site[, 
            names_with_co2])[i], data = site)
    }
    site$H <- qcClean(site$H, site$qc_H, qcFlag)
    if (!is.null(thresholdList$H) && is.null(timesList$H)) {
        site <- cleanVar(x = "H", data = site, lessThan = thresholdList$H[1], 
            greaterThan = thresholdList$H[2])
        site <- cleanSecondVar(x = "H", y = "qc_H", data = site)
    }
    else if (!is.null(timesList$H) && is.null(thresholdList$H)) {
        site$H <- sdClean(site$H, timesList$H)
        site <- cleanSecondVar(x = "H", y = "qc_H", data = site)
    }
    else if (!is.null(timesList$H) && !is.null(thresholdList$H)) {
        site$H <- sdClean(site$H, timesList$H)
        site <- cleanVar(x = "H", data = site, lessThan = thresholdList$H[1], 
            greaterThan = thresholdList$H[2])
        site <- cleanSecondVar(x = "H", y = "qc_H", data = site)
    }
    site$LE <- qcClean(site$LE, site$qc_LE, qcFlag)
    if (!is.null(thresholdList$LE) && is.null(timesList$LE)) {
        site <- cleanVar(x = "LE", data = site, lessThan = thresholdList$LE[1], 
            greaterThan = thresholdList$LE[2])
        site <- cleanSecondVar(x = "LE", y = "qc_LE", data = site)
    }
    else if (!is.null(timesList$LE) && is.null(thresholdList$LE)) {
        site$LE <- sdClean(site$LE, timesList$LE)
        site <- cleanSecondVar(x = "LE", y = "qc_LE", data = site)
    }
    else if (!is.null(timesList$LE) && !is.null(thresholdList$LE)) {
        site$LE <- sdClean(site$LE, timesList$LE)
        site <- cleanVar(x = "LE", data = site, lessThan = thresholdList$LE[1], 
            greaterThan = thresholdList$LE[2])
        site <- cleanSecondVar(x = "LE", y = "qc_LE", data = site)
    }
    site$Tau <- qcClean(site$Tau, site$qc_Tau, qcFlag)
    if (!is.null(thresholdList$Tau) && is.null(timesList$Tau)) {
        site <- cleanVar(x = "Tau", data = site, lessThan = thresholdList$Tau[1], 
            greaterThan = thresholdList$Tau[2])
        site <- cleanSecondVar(x = "Tau", y = "qc_Tau", data = site)
    }
    else if (!is.null(timesList$Tau) && is.null(thresholdList$Tau)) {
        site$Tau <- sdClean(site$Tau, timesList$Tau)
        site <- cleanSecondVar(x = "Tau", y = "qc_Tau", data = site)
    }
    else if (!is.null(timesList$Tau) && !is.null(thresholdList$Tau)) {
        site$Tau <- sdClean(site$Tau, timesList$Tau)
        site <- cleanVar(x = "Tau", data = site, lessThan = thresholdList$Tau[1], 
            greaterThan = thresholdList$Tau[2])
        site <- cleanSecondVar(x = "Tau", y = "qc_Tau", data = site)
    }
    site$h2o_flux <- qcClean(site$h2o_flux, site$qc_h2o_flux, 
        qcFlag)
    if (!is.null(thresholdList$h2o) && is.null(timesList$h2o)) {
        site <- cleanVar(x = "h2o_flux", data = site, lessThan = thresholdList$h2o[1], 
            greaterThan = thresholdList$h2o[2])
        site <- cleanSecondVar(x = "h2o_flux", y = "qc_h2o_flux", 
            data = site)
    }
    else if (!is.null(timesList$h2o) && is.null(thresholdList$h2o)) {
        site$h2o_flux <- sdClean(site$h2o_flux, timesList$h2o)
        site <- cleanSecondVar(x = "h2o_flux", y = "qc_h2o_flux", 
            data = site)
    }
    else if (!is.null(timesList$h2o) && !is.null(thresholdList$h2o)) {
        site$h2o_flux <- sdClean(site$h2o_flux, timesList$h2o)
        site <- cleanVar(x = "h2o_flux", data = site, lessThan = thresholdList$h2o[1], 
            greaterThan = thresholdList$h2o[2])
        site <- cleanSecondVar(x = "h2o_flux", y = "qc_h2o_flux", 
            data = site)
    }
    site <- cleanSecondVar(x = "h2o_flux", y = "ET", data = site)
    site <- cleanSecondVar(x = "LE", y = "ET", data = site)
    if (all(is.na(site[[gas]]))) {
        print("All CO2 is NA.")
    }
    if (all(is.na(site$H))) {
        print("All H is NA.")
    }
    if (all(is.na(site$LE))) {
        print("All LE is NA.")
    }
    if (all(is.na(site$h2o_flux))) {
        print("All H2O is NA.")
    }
    if (all(is.na(site$Tau))) {
        print("All Tau is NA.")
    }
    if (all(is.na(site$ET))) {
        print("All ET is NA.")
    }
    if (plot) {
        if (write) {
            plots.after <- paste(dirname(outputFile), "/after.jpg", 
                sep = "")
            jpeg(plots.after, width = 1200, height = 800, res = 100)
        }
        else {
            dev.set(3)
        }
        par(mfrow = c(4, 3), oma = c(0, 0, 1.7, 0), mar = c(4, 
            4, 2, 2))
        plot(co2_flux ~ DOY, data = site, type = "p", pch = 16, 
            cex = 0.8)
        plot(H ~ DOY, data = site, col = 1, type = "p", pch = 16, 
            cex = 0.8)
        plot(LE ~ DOY, data = site, col = 1, type = "p", pch = 16, 
            cex = 0.8)
        plot(Tau ~ DOY, data = site, col = 1, type = "p", pch = 16, 
            cex = 0.8)
        plot(h2o_flux ~ DOY, data = site, col = 1, type = "p", 
            pch = 16, cex = 0.8)
        plot(ET ~ DOY, data = site, col = 1, type = "p", pch = 16, 
            cex = 0.8)
        plot(wind_speed ~ DOY, data = site, col = 1, type = "p", 
            pch = 16, cex = 0.8)
        plot(wind_dir ~ DOY, data = site, col = 1, type = "p", 
            pch = 16, cex = 0.8)
        plot(u. ~ DOY, data = site, col = 1, type = "p", pch = 16, 
            cex = 0.8)
        plot(air_temperature ~ DOY, data = site, col = 1, type = "p", 
            pch = 16, cex = 0.8)
        plot(VPD ~ DOY, data = site, col = 1, type = "p", pch = 16, 
            cex = 0.8)
        mtext("After cleaning", outer = TRUE, cex = 1)
        if (write) {
            dev.off(3)
        }
    }
    if (write) {
        if (.Platform$OS.type == "unix") {
            if (missing(outputFile)) {
                stop("No output file given while trying to write clean output. Please provide a file name.", 
                  call. = FALSE)
            }
            if (!is.data.frame(data)) {
                header <- paste("head -n 3", file, ">", outputFile, 
                  sep = " ")
                system(header)
            }
            drop <- c("timestamp", "yday", "year", "month", "hour")
            site <- site[, !names(site) \%in\% drop]
            colNames = TRUE
            app = FALSE
            if (!is.data.frame(data)) 
                colNames = FALSE
            if (!is.data.frame(data)) 
                app = TRUE
            write.table(site, outputFile, col.names = colNames, 
                row.names = FALSE, sep = ",", quote = FALSE, 
                append = app, na = as.character(na.value))
        }
        else if (.Platform$OS.type == "windows") {
            if (missing(outputFile)) {
                stop("No output file given while trying to write clean output. Please provide a file name.", 
                  call. = FALSE)
            }
            drop <- c("timestamp", "yday", "year", "month", "hour")
            site <- site[, !names(site) \%in\% drop]
            app = FALSE
            if (!is.data.frame(data)) 
                app = TRUE
            write.table(site, outputFile, col.names = TRUE, row.names = FALSE, 
                sep = ",", quote = FALSE, append = app, na = as.character(na.value))
        }
    }
    else {
        return(site)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
