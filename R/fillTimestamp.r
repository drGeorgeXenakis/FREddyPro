fillTimestamp <-
function (data, timestamp = "timestamp", tz = "GMT", timediff = 30, 
    units = "min") 
{
    data <- data.frame(data)
    if ("time" %in% names(data)) 
        data[["time"]] <- as.character(data[["time"]])
    if (timestamp %in% names(data)) {
        ct <- continuity(data, timestamp = timestamp, timediff = timediff, 
            units = units, return = T)
        if (is.null(ct)) {
            cat("No discontinuity")
        }
        else {
            for (i in 1:nrow(ct)) {
                rN <- which(data[[timestamp]] == ct[[1]][i])
                timeStart <- as.POSIXct(data[rN, timestamp], 
                  tz = tz)
                timeEnd <- as.POSIXct(data[rN + 1, timestamp], 
                  tz = tz) - minutes(30)
                timeDif <- difftime(timeEnd, timeStart, units = "mins")
                print(timeDif)
                numberHalfHours = timeDif[[1]]/30
                emptyRows <- createEmptyRows(data, ts = timestamp, 
                  nHH = numberHalfHours, tS = timeStart, tE = timeEnd)
                data <- rbind.data.frame(data[1:(rN), ], emptyRows, 
                  data[(rN + 1):nrow(data), ])
            }
        }
    }
    else {
        cat("Please give a timestamp")
    }
    if ("time" %in% names(data)) 
        data[["time"]] <- hms::as_hms(data[["time"]])
    data <- as_tibble(data)
    return(data)
}
