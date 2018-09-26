exportFootprintPoints <-
function (ftp, xcoord, ycoord) 
{
    x = t(apply(ftp[[2]], 2, rev))
    y = t(apply(ftp[[3]], 2, rev))
    z = t(apply(ftp[[1]], 2, rev))
    df = data.frame(x = numeric(length(x)), y = numeric(length(y)), 
        z = numeric(length(cumsum(z))))
    df$z = as.vector(z)
    df$x = as.vector(x)
    df$y = as.vector(y)
    df$x = df$x + xcoord
    df$y = df$y + ycoord
    return(df)
}
.__global__ <-
"."
