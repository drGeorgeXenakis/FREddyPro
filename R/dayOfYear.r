dayOfYear <-
function (data, timestamp) 
{
    data$doy <- lubridate::yday(timestamp) + lubridate::hour(timestamp)/24 + 
        lubridate::minute(timestamp)/(24 * 60) + lubridate::second(timestamp)/(24 * 
        60 * 60)
    return(data)
}
