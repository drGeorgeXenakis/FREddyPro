ustarThreshold <-
function (data, sunset = 19, sunrise = 6) 
{
    data = createTimestamp(data)
    data$air_temperature = data$air_temperature - 273.15
    night = which(data$hour >= sunset | data$hour <= sunrise)
    data_night = data[night, ]
    temp.breaks = as.vector(quantile(data_night$air_temperature, 
        seq(0, 1, length = 8), na.rm = TRUE))
    data_night$temp.class = cut(data_night$air_temperature, breaks = temp.breaks, 
        include.lowest = TRUE)
    unique.tc = unique(data_night$temp.class)
    t = data.frame(DOY = numeric(), ustra.class = factor())
    options(warn = -1)
    for (i in 1:length(unique.tc)) {
        index <- which(data_night$temp.class == unique.tc[i])
        if (length(index) != 0) {
            t.df = data_night[index, c("DOY", "u.", "temp.class")]
            ustar.breaks = as.vector(quantile(t.df$u., seq(0, 
                1, length = 21), na.rm = TRUE))
            t.df$ustar.class = cut(t.df$u., breaks = ustar.breaks, 
                include.lowest = TRUE)
            t = rbind(t, t.df[, c("DOY", "ustar.class")])
        }
    }
    data_night = merge(data_night, t, by.x = "DOY", by.y = "DOY", 
        all.x = TRUE)
    df = aggregate(cbind(co2_flux, u.) ~ temp.class + ustar.class, 
        data = data_night, mean)
    untc = unique(df$temp.class)
    ust = array(NA, dim = c(0, length(untc)))
    for (i in 1:length(untc)) {
        index <- which(df$temp.class == untc[i])
        sub.df <- df[index, ]
        sub.df$ustar.class.midpoints <- midpoints(sub.df$ustar.class)
        higher <- which(sub.df$u. > quantile(sub.df$u., probs = 0.1, 
            na.rm = TRUE))
        M = mean(sub.df$co2_flux[higher], na.rm = TRUE)
        ust[i] = sub.df$u.[which(sub.df$co2_flux[which(sub.df$co2_flux >= 
            M * 0.99)] == max(sub.df$co2_flux[which(sub.df$co2_flux >= 
            M * 0.99)]))]
    }
    ustar.threshold = median(ust, na.rm = TRUE)
    return(ustar.threshold)
}
