.__global__ <-
"."
fillTimestamp <-
function (data, timestamp = "timestamp", flux = FALSE, timedif = 30, 
    year = FALSE) 
{
    shift_timestamp <- function(ts, chg, amount) {
        ts = unclass(as.POSIXlt(ts))
        ts[[chg]] <- ts[[chg]] + amount
        return(.POSIXlt(ts))
    }
    data = createTimestamp(data, timestamp = timestamp)
    if (year) {
        start = as.POSIXct(paste0(year(data[[timestamp]][1]), 
            "-", sprintf("%02d", 1), "-", sprintf("%02d", 1), 
            " ", sprintf("%02d", 0), ":", sprintf("%02d", 0)), 
            format = "%Y-%m-%d %H:%M", tz = "UTC")
    }
    else {
        start = as.POSIXct(paste0(year(data[[timestamp]][1]), 
            "-", sprintf("%02d", month(data[[timestamp]][1])), 
            "-", sprintf("%02d", 1), " ", sprintf("%02d", 0), 
            ":", sprintf("%02d", 0)), format = "%Y-%m-%d %H:%M", 
            tz = "UTC")
    }
    l <- list(`1` = 31, `2` = 28, `3` = 31, `4` = 30, `5` = 31, 
        `6` = 30, `7` = 31, `8` = 31, `9` = 30, `10` = 31, `11` = 30, 
        `12` = 31)
    if (year) {
        end = as.POSIXct(paste0(year(data[[timestamp]][nrow(data)]), 
            "-", sprintf("%02d", 12), "-", sprintf("%02d", l[[as.character(month(data[[timestamp]][nrow(data)]))]]), 
            " ", sprintf("%02d", 23), ":", sprintf("%02d", 30)), 
            format = "%Y-%m-%d %H:%M", tz = "UTC")
    }
    else {
        end = as.POSIXct(paste0(year(data[[timestamp]][nrow(data)]), 
            "-", sprintf("%02d", month(data[[timestamp]][nrow(data)])), 
            "-", sprintf("%02d", l[[as.character(month(data[[timestamp]][nrow(data)]))]]), 
            " ", sprintf("%02d", 23), ":", sprintf("%02d", 30)), 
            format = "%Y-%m-%d %H:%M", tz = "UTC")
    }
    if (data[[timestamp]][1] > start) {
        temprow <- matrix(c(rep.int(NA, length(data))), nrow = 1, 
            ncol = length(data))
        newrow <- data.frame(temprow)
        colnames(newrow) <- colnames(data)
        newrow[[timestamp]] <- start
        data <- rbind(newrow, data)
    }
    if (data[[timestamp]][nrow(data)] < end) {
        temprow <- matrix(c(rep.int(NA, length(data))), nrow = 1, 
            ncol = length(data))
        newrow <- data.frame(temprow)
        colnames(newrow) <- colnames(data)
        newrow[[timestamp]] <- end
        data <- rbind(data, newrow)
    }
    t = invisible(continuity(data, timestamp = timestamp))
    while (!is.character(t)) {
        index = which(data[[timestamp]] == t[1])
        f1 = data[1:(index - 1), ]
        f2 = data[(index):nrow(data), ]
        temprow <- matrix(c(rep.int(NA, length(data))), nrow = 1, 
            ncol = length(data))
        newrow <- data.frame(temprow)
        colnames(newrow) <- colnames(data)
        k <- shift_timestamp(data[[timestamp]][index], "min", 
            -timedif)
        newrow[[timestamp]] = k
        if (flux) {
            newrow[["date"]] = paste(year(as.character(k)), "-", 
                sprintf("%02d", month(as.character(k))), "-", 
                sprintf("%02d", day(as.character(k))), sep = "")
            newrow[["time"]] = paste(sprintf("%02d", hour(as.character(k))), 
                ":", sprintf("%02d", minute(as.character(k))), 
                sep = "")
            newrow[["DOY"]] = round(yday(as.character(k)) + hour(as.character(k))/24 + 
                minute(as.character(k))/(24 * 60), 3)
        }
        data = rbind(f1, newrow)
        data = rbind(data, f2)
        t = invisible(continuity(data, timestamp = timestamp))
    }
    return(data)
}
