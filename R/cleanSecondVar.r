cleanSecondVar <-
function (x, y, data) 
{
    clean <- which(is.na(data[[x]]) & !is.na(data[[y]]))
    data[[y]][clean] <- NA
    return(data)
}
