buildTimestamp <-
function (data, doy, decimal.hour, origin = "2016-01-01") 
{
    dayOfYear = data[[doy]]
    d.hour = data[[decimal.hour]]
    hour = floor(d.hour)
    minutes = (d.hour - hour) * 60
    seconds = rep(0, length(hour))
    timestamp = paste(as.Date(dayOfYear - 1, origin = origin), 
        " ", sprintf("%02d", hour), ":", sprintf("%02d", minutes), 
        ":", sprintf("%02d", seconds), sep = "")
    spl = cbind(as.POSIXlt(timestamp), as.data.frame(data))
    names(spl) <- c("timestamp", names(as.data.frame(data)))
    return(spl)
}
.__global__ <-
"."
