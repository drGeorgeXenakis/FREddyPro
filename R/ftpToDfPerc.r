ftpToDfPerc <-
function (ftp, percent) 
{
    x = ftp[[2]]
    y = ftp[[3]]
    z = ftp[[1]]
    df = data.frame(x = numeric(length(x)), y = numeric(length(y)), 
        z = numeric(length(cumsum(z))))
    df$z = as.vector(z)
    df$x = as.vector(x)
    df$y = as.vector(y)
    t <- df[order(-df$z), ]
    t$cumz <- cumsum(t$z) * 100
    t$cumz = ifelse(t$cumz <= percent, t$cumz, NA)
    return(t)
}
