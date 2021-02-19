.__global__ <-
"."
createTimestamp <-
function (data, timestamp = NULL, timeZone = "GMT") 
{
    if (is.null(timestamp)) {
        data$timestamp <- paste(data$date, data$time, sep = " ")
        data$timestamp <- as.POSIXct(data$timestamp, tz = timeZone)
        data$year <- year(data$timestamp)
        data$month <- month(data$timestamp)
        data$yday <- yday(data$timestamp)
        data$hour <- hour(data$timestamp) + minute(data$timestamp)/60
        data$minute <- minute(data$timestamp)
        data$second <- second(data$timestamp)
    }
    else {
        data[[timestamp]] <- as.POSIXct(data[[timestamp]], tz = timeZone)
        data$year <- year(data[[timestamp]])
        data$month <- month(data[[timestamp]])
        data$yday <- yday(data[[timestamp]])
        data$hour <- hour(data[[timestamp]]) + minute(data[[timestamp]])/60
        data$minute <- minute(data[[timestamp]])
        data$second <- second(data[[timestamp]])
    }
    return(data)
}
