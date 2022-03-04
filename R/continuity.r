continuity <-
function (data, timestamp = NULL, timediff = 30, units = "min", 
    return = FALSE) 
{
    if (is.null(timestamp)) {
        data <- data %>% createTimestamp(data = .)
    }
    else {
        data <- data %>% createTimestamp(data = ., timestamp = timestamp)
    }
    data <- data %>% mutate(dt = c(difftime(tail(timestamp, -1), 
        head(timestamp, -1), units = units), 0))
    nDiscontinuity <- data %>% filter(dt > timediff) %>% select(timestamp) %>% 
        nrow
    if (nDiscontinuity != 0) {
        print(paste("Discontinuity in ", nDiscontinuity, " instances", 
            sep = ""), quote = F)
        discont <- data %>% filter(dt > timediff) %>% select(timestamp)
        discont %>% print
    }
    else {
        print("No discontinuity", quote = F)
    }
    if (nDiscontinuity != 0 & return) 
        return(discont)
}
