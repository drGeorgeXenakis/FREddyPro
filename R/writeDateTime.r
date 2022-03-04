.__global__ <-
"."
writeDateTime <-
function (df, timestamp = "timestamp", date = "date", time = "time", 
    doy = "DOY") 
{
    if (is.null(df[[timestamp]])) {
        stop("Timestamp does not exist")
    }
    else {
        if (any(is.na(df[[date]]))) {
            nR <- which(is.na(df[[date]]))
            if (date %in% names(df)) 
                df[nR, date] <- as.factor(as.Date(df[nR, timestamp]))
            if (time %in% names(df)) 
                df[nR, time] <- format(df[nR, timestamp], format = "%H:%M")
            if (doy %in% names(df)) 
                df[nR, doy] <- doy(df[nR, timestamp])
        }
    }
    return(df)
}
