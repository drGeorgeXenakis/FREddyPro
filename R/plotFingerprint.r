plotFingerprint <-
function (var, doy, hour, step = 2, xlab = "Hour", ylab = "Day or Year", 
    ...) 
{
    new.red = rgb(255/255, 0/255, 0/255)
    new.yellow = rgb(255/255, 255/255, 0/255)
    new.blue = rgb(0/255, 0/255, 255/255)
    rgb.palette <- colorRampPalette(c(new.blue, new.yellow, new.red))
    data <- data.frame(cbind(doy, hour, var))
    m <- data.matrix(reshape(data, v.names = "var", timevar = "doy", 
        idvar = "hour", direction = "wide"))
    doy.lim = seq(range(doy)[1], range(doy)[2], step)
    at.seq = seq(0, max(doy.lim)/range(doy)[2], length = length(doy.lim))
    seq(range(doy)[1], range(doy)[2], 12)
    filled.contour(m, color.palette = rgb.palette, ylab = "Day of Year", 
        xlab = "Hour", plot.axes = {
            axis(side = 2, at = at.seq, labels = as.character(doy.lim))
            axis(side = 1, at = seq(0, 1, 1/23), labels = as.character(seq(0, 
                23, 1)))
        }, ...)
}
