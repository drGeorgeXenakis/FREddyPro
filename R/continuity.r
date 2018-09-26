continuity <-
function (data, timestamp = NULL, timediff = 30) 
{
    if (is.null(timestamp)) {
        data = createTimestamp(data)
    }
    else {
        data = createTimestamp(data, timestamp = timestamp)
    }
    data$dayOfYear = data$yday + data$hour/24
    dt = timediff/(24 * 60)
    data$diff[2:nrow(data)] = diff(data$dayOfYear)
    data$dci = ifelse(round(data$diff, 5) == round(dt, 5), 0, 
        1)
    dc = which(data$dci == 1)
    if (length(dc) != 0) {
        print(paste("Discontinuity in ", length(dc), " instances", 
            sep = ""), quote = F)
        if (is.null(timestamp)) {
            print(data[["timestamp"]][dc], quote = FALSE)
        }
        else {
            print(data[[timestamp]][dc], quote = FALSE)
        }
    }
    else {
        print("No discontinuity", quote = F)
    }
}
.__global__ <-
"."
