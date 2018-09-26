.__global__ <-
"."
sdClean <-
function (var, p) 
{
    positive <- which(var > 0)
    negative <- which(var < 0)
    m.positive <- mean(var[positive], na.rm = TRUE)
    m.negative <- mean(var[negative], na.rm = TRUE)
    sd.positive <- sd(var[positive], na.rm = TRUE)
    sd.negative <- sd(var[negative], na.rm = TRUE)
    filter.positive <- m.positive + p * sd.positive
    filter.negative <- m.negative - p * sd.negative
    rm.positive <- which(var > filter.positive)
    rm.negative <- which(var < filter.negative)
    var[rm.positive] <- NA
    var[rm.negative] <- NA
    return(var)
}
