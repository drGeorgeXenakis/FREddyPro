.__global__ <-
"."
readEddyPro <-
function (dataFile, na = "NaN", sep = ",", ...) 
{
    df <- readFile(dataFile, 3, 1, na = na, sep = sep, ...)
    return(df)
}
