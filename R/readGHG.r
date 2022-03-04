.__global__ <-
"."
readGHG <-
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
    flx <- readr::read_delim(file = unz(file, paste0(tools::file_path_sans_ext(basename(file)), 
        ".data")), col_names = F, skip = 9, delim = "\t")
    n.flx <- readr::read_delim(file = unz(file, paste0(tools::file_path_sans_ext(basename(file)), 
        ".data")), col_names = T, skip = 7, delim = "\t", n_max = 1)
    bmt <- readr::read_delim(file = unz(file, paste0(tools::file_path_sans_ext(basename(file)), 
        "-biomet.data")), col_names = F, skip = 6, delim = "\t")
    n.bmt <- readr::read_delim(file = unz(file, paste0(tools::file_path_sans_ext(basename(file)), 
        "-biomet.data")), col_names = T, skip = 5, delim = "\t", 
        n_max = 1)
    names(flx) <- names(n.flx)
    names(bmt) <- names(n.bmt)
    flx <- crTimeSt(flx, TimeZone = TimeZone, flux = TRUE)
    bmt <- crTimeSt(bmt, TimeZone = TimeZone, flux = FALSE)
    return(list(flux = flx, biomet = bmt))
}
