cleanVarL <-
function (data, x, y = NULL, lessThan = NULL) 
{
    clean <- which(data[[x]] <= lessThan)
    if (is.null(y)) {
        data[[x]][clean] <- NA
    }
    else {
        data[[y]][clean] <- NA
    }
    return(data)
}
