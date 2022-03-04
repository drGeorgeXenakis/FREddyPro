.__global__ <-
"."
readFile <-
function (dataFile, nSkip = 0, nSkipNames = 0, header = FALSE, 
    reverse = FALSE, sep = ",", na = "NaN", ...) 
{
    if (!reverse) {
        if (nSkip == 0) {
            df <- readr::read_delim(file = dataFile, col_names = TRUE, 
                delim = sep, na = na, ...)
        }
        else {
            df <- readr::read_delim(file = dataFile, col_names = FALSE, 
                skip = nSkip, delim = sep, na = na, ...)
            if (nSkipNames != 0) {
                n <- readr::read_delim(file = dataFile, col_names = TRUE, 
                  skip = nSkipNames, n_max = 1, delim = sep, 
                  na = na, ...)
                names(df) <- names(n)
            }
        }
    }
    else {
        df <- readr::read_delim(file = dataFile, col_names = F, 
            skip = nSkip, delim = sep, na = na, ...)
        n <- readr::read_delim(file = dataFile, col_names = T, 
            skip = nSkipNames, n_max = 1, na = na, delim = sep, 
            ...)
        names(df) <- names(n)
    }
    return(df)
}
