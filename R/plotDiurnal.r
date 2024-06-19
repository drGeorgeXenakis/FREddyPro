plotDiurnal <-
function (data, xVar = "co2_flux", hourX = "hour", dataY = NULL, 
    yVar = NULL, hourY = NULL, median = FALSE, quantiles = FALSE, 
    probs = c(0.050000000000000003, 0.94999999999999996), stdev = FALSE, 
    sterr = FALSE, legend = FALSE, legendSide = NULL, legendText = NULL, 
    type = "l", lty = c(1, 4), col = c(1, 2), cex.legend = 1, 
    horiz.legend = TRUE, ylab = "Mean", xlab = "Hour", ...) 
{
    if (length(type) == 1) 
        type = c(type, type, type, type)
    if (length(type) == 2) 
        type = c(type[1], type[2], type[1], type[2])
    if (length(type) == 3) 
        type = c(type[1], type[2], type[3], type[1])
    if (length(lty) == 1) 
        lty = c(lty, lty, lty, lty)
    if (length(lty) == 2) 
        lty = c(lty[1], lty[2], lty[1], lty[2])
    if (length(lty) == 3) 
        lty = c(lty[1], lty[2], lty[3], lty[1])
    if (length(col) == 1) 
        col = c(col, col, col, col)
    if (length(col) == 2) 
        col = c(col[1], col[2], col[1], col[2])
    if (length(col) == 3) 
        col = c(col[1], col[2], col[3], col[1])
    new.moccasin1 <- rgb(255, 228, 181, 225, maxColorValue = 255)
    new.moccasin2 <- rgb(255, 228, 181, 170, maxColorValue = 255)
    new.darkgrey1 <- rgb(169, 169, 169, 210, maxColorValue = 255)
    new.darkgrey2 <- rgb(169, 169, 169, 100, maxColorValue = 255)
    meanX = aggregate(data[[xVar]], list(data[[hourX]]), mean, 
        na.rm = TRUE)
    medianX = aggregate(data[[xVar]], list(data[[hourX]]), median, 
        na.rm = TRUE)
    quantileX = aggregate(data[[xVar]], list(data[[hourX]]), 
        quantile, probs = probs, na.rm = TRUE)
    sdX = aggregate(data[[xVar]], list(data[[hourX]]), sd, na.rm = T)
    lengthX = aggregate(data[[xVar]], list(data[[hourX]]), FUN = length)
    seX = sdX
    seX[[2]] = seX[[2]]/sqrt(lengthX[[2]])
    meanX = as.data.frame(meanX)
    medianX = as.data.frame(medianX)
    quantileX = as.data.frame(t(as.data.frame(t(quantileX))))
    sdX = as.data.frame(sdX)
    seX = as.data.frame(seX)
    par(xaxs = "i", yaxs = "i", mar = c(4, 5, 2, 2))
    plot(meanX[[2]] ~ meanX[[1]], lty = lty[1], type = type[1], 
        col = col[1], xlab = xlab, ylab = ylab, ...)
    if (median) {
        points(medianX[[2]] ~ medianX[[1]], col = col[2], lty = lty[2], 
            type = type[2], ...)
    }
    if (quantiles) {
        polygon(c(quantileX[[1]], rev(quantileX[[1]])), c(quantileX[[2]], 
            rev(quantileX[[3]])), col = new.moccasin1, border = NA)
        points(meanX[[2]] ~ meanX[[1]], lty = lty[1], type = type[1], 
            col = col[1], ...)
        if (median) {
            points(medianX[[2]] ~ medianX[[1]], lty = lty[2], 
                type = type[2], col = col[2], ...)
        }
    }
    if (stdev) {
        polygon(c(sdX[[1]], rev(sdX[[1]])), c(meanX[[2]] - sdX[[2]], 
            rev(meanX[[2]]) + rev(sdX[[2]])), col = new.moccasin1, 
            border = NA)
        points(meanX[[2]] ~ meanX[[1]], lty = lty[1], type = type[1], 
            col = col[1], ...)
        if (median) {
            points(medianX[[2]] ~ medianX[[1]], lty = lty[2], 
                type = type[2], col = col[2], ...)
        }
    }
    if (sterr) {
        polygon(c(seX[[1]], rev(seX[[1]])), c(meanX[[2]] - seX[[2]], 
            rev(meanX[[2]]) + rev(seX[[2]])), col = new.moccasin1, 
            border = NA)
        points(meanX[[2]] ~ meanX[[1]], lty = lty[1], type = type[1], 
            col = col[1], ...)
        if (median) {
            points(medianX[[2]] ~ medianX[[1]], lty = lty[2], 
                type = type[2], col = col[2], ...)
        }
    }
    if (!is.null(yVar)) {
        if (!is.null(dataY)) {
            data = dataY
        }
        else {
            hourY = hourX
        }
        meanY = aggregate(data[[yVar]], list(data[[hourY]]), 
            mean, na.rm = TRUE)
        medianY = aggregate(data[[yVar]], list(data[[hourY]]), 
            median, na.rm = TRUE)
        quantileX = aggregate(data[[yVar]], list(data[[hourY]]), 
            quantile, probs = probs, na.rm = TRUE)
        sdY = aggregate(data[[yVar]], list(data[[hourY]]), sd, 
            na.rm = T)
        lengthY = aggregate(data[[yVar]], list(data[[hourY]]), 
            FUN = length)
        seY = sdY
        seY[[2]] = seY[[2]]/sqrt(lengthY[[2]])
        meanY = as.data.frame(meanY)
        medianY = as.data.frame(medianY)
        quantileX = as.data.frame(t(as.data.frame(t(quantileX))))
        points(meanY[[2]] ~ meanY[[1]], lty = lty[3], type = type[3], 
            col = col[3], ...)
        if (median) {
            points(medianY[[2]] ~ medianY[[1]], col = col[4], 
                lty = lty[4], type = type[4], ...)
        }
        if (quantiles) {
            polygon(c(quantileX[[1]], rev(quantileX[[1]])), c(quantileX[[2]], 
                rev(quantileX[[3]])), col = new.darkgrey2, border = NA)
            points(meanY[[2]] ~ meanY[[1]], lty = lty[3], col = col[3], 
                type = type[3], ...)
            if (median) {
                points(medianY[[2]] ~ medianY[[1]], lty = lty[4], 
                  col = col[4], type = type[4], ...)
            }
        }
        if (stdev) {
            polygon(c(sdY[[1]], rev(sdY[[1]])), c(meanY[[2]] - 
                sdY[[2]], rev(meanY[[2]]) + rev(sdY[[2]])), col = new.darkgrey2, 
                border = NA)
            points(meanY[[2]] ~ meanY[[1]], lty = lty[1], type = type[1], 
                col = col[3], ...)
            if (median) {
                points(medianY[[2]] ~ medianY[[1]], lty = lty[4], 
                  type = type[4], col = col[4], ...)
            }
        }
        if (sterr) {
            polygon(c(seY[[1]], rev(seY[[1]])), c(meanY[[2]] - 
                seY[[2]], rev(meanY[[2]]) + rev(seY[[2]])), col = new.darkgrey2, 
                border = NA)
            points(meanY[[2]] ~ meanY[[1]], lty = lty[1], type = type[1], 
                col = col[3], ...)
            if (median) {
                points(medianY[[2]] ~ medianY[[1]], lty = lty[4], 
                  type = type[4], col = col[4], ...)
            }
        }
    }
    if (legend) {
        if (is.null(legendSide)) {
            print("You must give which side to draw the legend", 
                quote = FALSE)
        }
        else {
            if (!is.null(legendText)) {
                text1 = paste("Mean", legendText[1], sep = " ")
                text2 = paste("Median", legendText[1], sep = " ")
                text3 = paste("Mean", legendText[2], sep = " ")
                text4 = paste("Median", legendText[2], sep = " ")
                legend(legendSide, c(text1, text2, text3, text4), 
                  col = c(col[1], col[2], col[3], col[4]), lty = c(lty[1], 
                    lty[2], lty[3], lty[4]), horiz = horiz.legend, 
                  cex = cex.legend, bty = "n")
            }
            else {
                legend(legendSide, c("Mean", "Median"), col = c(col[1], 
                  col[2]), lty = c(lty[1], lty[2]), horiz = FALSE, 
                  bty = "n")
            }
        }
    }
}
