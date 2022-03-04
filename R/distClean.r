distClean <-
function (var, hour, df) 
{
    m <- aggregate(var ~ hour, data = df, mean)
    q <- as.data.frame(t(as.data.frame(t(aggregate(var ~ hour, 
        data = df, quantile, prob = c(0.05, 0.95))))))
    p <- merge(m, q, "hour")
    names(p)[c(3, 4)] <- c("quant5", "quant95")
    for (i in 1:nrow(p)) {
        var[which(hour == p$hour[i])] <- ifelse(var[which(hour == 
            p$hour[i])] < p$quant5[i], NA, var[which(hour == 
            p$hour[i])])
        var[which(hour == p$hour[i])] <- ifelse(var[which(hour == 
            p$hour[i])] > p$quant95[i], NA, var[which(hour == 
            p$hour[i])])
    }
    return(var)
}
