checkLeapYear <-
function (ts) 
{
    leapYear <- FALSE
    years <- 1900:2500
    yr <- as.numeric(strftime(ts, format = "%Y"))
    if (yr %in% years[leap_year(years)]) 
        leapYear <- TRUE
    return(leapYear)
}
