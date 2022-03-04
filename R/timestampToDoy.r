.__global__ <-
"."
timestampToDoy <-
function (ts, decimals = 3) 
{
    doy <- round(as.numeric(strftime(as.POSIXlt(ts), format = "%j")) + 
        as.numeric(strftime(as.POSIXlt(ts), format = "%H"))/24 + 
        as.numeric(strftime(as.POSIXlt(ts), format = "%M"))/(24 * 
            60) + as.numeric(strftime(as.POSIXlt(ts), format = "%S"))/(24 * 
        60 * 60), decimals)
    return(doy)
}
