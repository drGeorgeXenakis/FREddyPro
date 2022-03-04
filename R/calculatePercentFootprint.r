calculatePercentFootprint <-
function (ftp, percent = 99.99) 
{
    rotate <- function(x) t(apply(x, 2, rev))
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
    m <- xtabs(cumz ~ x + y, data = t)
    m.sub = ifelse(m <= percent, m, NA)
    f = ftp
    f[[1]] = rotate(rotate(rotate(unclass(m.sub))))
    return(f)
}
