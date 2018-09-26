.__global__ <-
"."
plotMonthly <-
function (data, var = "co2_flux", legend = FALSE, legendSide = NULL, 
    type = "o", col = 1, lty = 1, yaxt.out = NULL, yaxt.in = NULL, 
    xaxt.out = NULL, xaxt.in = NULL, axis1.in = FALSE, at1.in = NULL, 
    axis2.in = FALSE, at2.in = NULL, axis1.out = FALSE, at1.out = NULL, 
    axis2.out = FALSE, at2.out = NULL, ...) 
{
    month <- c("January", "February", "March", "April", "May", 
        "June", "July", "August", "September", "October", "November", 
        "December")
    agg = aggregate(data[[var]], list(hour = data$hour, month = data$month), 
        mean, na.rm = TRUE)
    parRaw <- ifelse(length(unique(data$month)) == 12, 3, length(which(unique(data$month)%%3 == 
        0)))
    parCol <- ifelse(((length(unique(data$month)) - 4)%%4) == 
        0, 4, ifelse(length(unique(data$month)) < 4, length(unique(data$month)), 
        4))
    par(mfrow = c(parRaw, parCol), mar = c(0, 0.8, 0, 0), oma = c(5, 
        5, 5, 5))
    for (i in unique(data$month)[!is.na(unique(data$month))]) {
        if (i == 1 | i == 5 | i == 9) {
            plot(agg[which(agg$month == i), 3] ~ agg[which(agg$month == 
                i), 1], type = type, col = col, lty = lty, yaxt = yaxt.out, 
                xaxt = xaxt.out, ...)
            axis(1, at1.out, labels = axis1.out)
            axis(2, at2.out, labels = axis2.out)
            abline(h = 0, lty = 2)
        }
        else {
            plot(agg[which(agg$month == i), 3] ~ agg[which(agg$month == 
                i), 1], type = type, col = col, lty = lty, yaxt = yaxt.in, 
                xaxt = xaxt.in, ...)
            axis(1, at1.in, labels = axis1.in)
            axis(2, at2.in, labels = axis2.in)
            abline(h = 0, lty = 2)
        }
        if (legend) {
            if (!is.null(legendSide)) {
                legend(legendSide, month[i], lty = -1, pch = -1, 
                  bty = "n", cex = 1)
            }
            else if (is.null(legendSide)) {
                print("You must give which side to draw the legend", 
                  quote = FALSE)
            }
        }
    }
}
