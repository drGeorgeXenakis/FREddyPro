createTimestamp <-
function (data, timestamp = NULL, tz = "GMT") 
{
    if (is.null(timestamp)) {
        data <- data %>% mutate(timestamp = as.POSIXct(paste(date, 
            time, sep = " "), tz = tz), year = year(timestamp), 
            month = month(timestamp), yday = yday(timestamp), 
            hour = hour(timestamp) + minute(timestamp)/60, minute = minute(timestamp), 
            second = second(timestamp))
    }
    else {
        data <- data %>% mutate(timestamp = as.POSIXct(timestamp, 
            tz = tz), year = year(timestamp), month = month(timestamp), 
            yday = yday(timestamp), hour = hour(timestamp) + 
                minute(timestamp)/60, minute = minute(timestamp), 
            second = second(timestamp))
    }
    return(data)
}
