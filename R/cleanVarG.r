cleanVarG <-
function (data, x, y = NULL, greaterThan = NULL) 
{
    clean <- which(data[[x]] >= greaterThan)
    if (is.null(y)) {
        data[[x]][clean] <- NA
    }
    else {
        data[[y]][clean] <- NA
    }
    return(data)
}
