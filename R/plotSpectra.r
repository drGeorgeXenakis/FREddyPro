.__global__ <-
"."
plotSpectra <-
function (data, gas, xlab = "Frequency [Hz]", ylab = "Spectrum", 
    avgT = FALSE, predicted = FALSE, type = c("o", "o", "o"), 
    col = c(1, 2, 4), pch = c(1, 1, 1), na.value = "NaN", ...) 
{
    if (!is.data.frame(data)) {
        data <- readFile(data, 3, 2, na = na.value)
    }
    else {
        data <- data
    }
    column = grep(gas, names(data))
    if (length(col) == 1) 
        col = c(col, col, col)
    if (length(col) == 2) 
        col = c(col[1], col[2], col[2])
    if (length(type) == 1) 
        type = c(type, type, type)
    if (length(type) == 2) 
        type = c(type[1], type[2], type[2])
    if (length(pch) == 1) 
        pch = c(pch, pch, pch)
    if (length(pch) == 2) 
        pch = c(pch[1], pch[2], pch[2])
    plot(data[[column[1]]] ~ data[[column[[1]] - 1]], log = "xy", 
        xlab = xlab, ylab = ylab, col = col[1], type = type[1], 
        pch = pch[1], xaxt = "n", ...)
    axis(1, at = c(0.001, 0.01, 0.10000000000000001, 1, 10), 
        labels = c("0.001", "0.01", "0.1", "1", "10"))
    if (avgT) {
        points(data[[column[1] - 1]] ~ data[[column[[1]] - 1]], 
            col = col[2], type = type[2], pch = pch[2], ...)
    }
    if (predicted) {
        points(data[[column[2]]] ~ data[[column[[1]] - 1]], col = col[3], 
            type = type[3], pch = pch[3], ...)
    }
}
