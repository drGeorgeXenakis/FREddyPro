.__global__ <-
"."
readBiomet <-
function (dataFile, na = "NaN", sep = ",", ...) 
{
    df <- readFile(dataFile, 2, rev = T, na = na, sep = sep, 
        ...)
    return(df)
}
