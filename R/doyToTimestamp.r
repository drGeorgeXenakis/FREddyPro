.__global__ <-
"."
doyToTimestamp <-
function (doy, originYear = 2000, timeZone = "GMT") 
{
    ts <- as.POSIXct(as.Date(doy - 1, origin = paste0(originYear, 
        "-01-01"), tz = timeZone))
    return(ts)
}
