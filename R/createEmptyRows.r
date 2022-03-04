createEmptyRows <-
function (df, ts = timestamp, nHH, tS, tE) 
{
    if (nHH == 0) {
        tst <- tE
    }
    else {
        tst <- seq.POSIXt(tS, tE, "30 min")
    }
    f <- function(x) NA
    nR <- df %>% mutate_all(f) %>% slice(2:length(tst))
    nR[[ts]] <- tst[2:length(tst)]
    if ("date" %in% names(df)) 
        nR[["date"]] <- as.Date(nR[[ts]])
    if ("time" %in% names(df)) 
        nR[["time"]] <- paste0(sprintf("%02d", hour(nR[[ts]])), 
            ":", sprintf("%02d", minute(nR[[ts]])), ":", sprintf("%02d", 
                second(nR[[ts]])))
    if ("DOY" %in% names(df)) 
        nR[["DOY"]] <- timestampToDoy(nR[[ts]])
    return(nR)
}
