.__global__ <-
"."
readFile <-
function (dataFile, nSkip = 0, nSkipNames = 0, header = FALSE, 
    reverse = FALSE, sep = ",", na = "NaN", ...) 
{
    if (!reverse) {
        if (nSkip == 0) {
            df <- read.table(dataFile, header = header, sep = sep, 
                ...)
        }
        else {
            df <- read.table(dataFile, header = FALSE, skip = nSkip, 
                sep = sep, ...)
            if (nSkipNames != 0) {
                n <- read.table(dataFile, header = TRUE, skip = nSkipNames, 
                  nrows = 1, sep = sep, fill = TRUE)
                names(df) <- names(n)
            }
        }
    }
    else {
        n <- read.table(dataFile, header = TRUE, nrows = 1, sep = sep, 
            fill = TRUE)
        df <- read.table(dataFile, header = FALSE, skip = nSkip, 
            sep = sep, ...)
        names(df) <- names(n)
    }
    df[df == na] <- NA
    return(df)
}
