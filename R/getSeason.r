getSeason <-
function (input.date) 
{
    require(lubridate)
    numeric.date <- 100 * month(input.date) + day(input.date)
    cuts <- base::cut(numeric.date, breaks = c(0, 319, 620, 921, 
        1220, 1231))
    levels(cuts) <- c("Winter", "Spring", "Summer", "Fall", "Winter")
    return(cuts)
}
