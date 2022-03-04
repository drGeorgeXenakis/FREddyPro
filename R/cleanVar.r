cleanVar <-
function (x, data, lessThan = NULL, greaterThan = NULL) 
{
    clean <- which(data[[x]] >= greaterThan | data[[x]] <= lessThan)
    data[[x]][clean] <- NA
    return(data)
}
