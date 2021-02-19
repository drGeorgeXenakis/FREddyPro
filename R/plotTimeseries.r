.__global__ <-
"."
plotTimeseries <-
function (data1, limList = NULL, data2 = NULL, step = 1, legendText = NULL, 
    ...) 
{
    if (is.null(limList)) {
        limList$DOY = c(0, 365)
    }
    else if (!is.null(limList) && is.null(limList$DOY)) {
        limList$DOY = c(0, 365)
    }
    if (length(grep("Rn_1_1_1", names(data1))) == 0) {
        par(mfrow = c(6, 1), oma = c(4, 2, 2, 2), mar = c(0, 
            5, 0, 4), cex = 0.8, xaxs = "i", yaxs = "i")
    }
    else {
        par(mfrow = c(7, 1), oma = c(4, 2, 2, 2), mar = c(0, 
            5, 0, 4), cex = 0.8, xaxs = "i", yaxs = "i")
    }
    plot(co2_flux ~ DOY, data = data1, xlim = limList$DOY, ylim = limList$co2, 
        col = 1, xlab = "", ylab = "F"[c] ~ " (umol m"^-2 ~ " s"^-1 ~ 
            ")", xaxt = "n", ...)
    if (!is.null(data2)) {
        points(co2_flux ~ DOY, data = data2, col = 2, ...)
    }
    axis(1, at = seq(limList$DOY[1], limList$DOY[2], step), labels = FALSE)
    axis(3, at = seq(limList$DOY[1], limList$DOY[2], step), labels = FALSE)
    axis(4, labels = FALSE)
    abline(h = 0, lty = 3, lwd = 1.5)
    if (!is.null(legendText)) {
        legend("topleft", c(legendText[1], legendText[2]), col = c(1, 
            2), lty = 1, cex = 0.8, horiz = TRUE, bty = "n")
    }
    plot(H ~ DOY, data = data1, xlim = limList$DOY, ylim = limList$H, 
        col = 1, xlab = "", ylab = "", xaxt = "n", yaxt = "n", 
        xaxt = "n", ...)
    if (!is.null(data2)) {
        points(H ~ DOY, data = data2, xlim = c(201, 238), col = 2, 
            ...)
    }
    axis(1, at = seq(limList$DOY[1], limList$DOY[2], step), labels = FALSE)
    axis(2, labels = FALSE)
    axis(4, labels = TRUE)
    mtext(4, text = expression(paste("H (W m"^"-2", ")")), cex = 0.8, 
        outer = FALSE, line = 3.5)
    abline(h = 0, lty = 3, lwd = 1.5)
    if (!is.null(legendText)) {
        legend("topleft", c(legendText[1], legendText[2]), col = c(1, 
            2), lty = 1, cex = 0.8, horiz = TRUE, bty = "n")
    }
    plot(LE ~ DOY, data = data1, xlim = limList$DOY, ylim = limList$LE, 
        col = 1, xlab = "Day of Year", ylab = "LE (W m"^-2 ~ 
            ")", xaxt = "n", xaxt = "n", ...)
    if (!is.null(data2)) {
        points(LE ~ DOY, data = data2, xlim = c(201, 238), col = 2, 
            ...)
    }
    axis(1, at = seq(limList$DOY[1], limList$DOY[2], step), labels = FALSE)
    axis(4, labels = FALSE)
    abline(h = 0, lty = 3, lwd = 1.5)
    if (!is.null(legendText)) {
        legend("topleft", c(legendText[1], legendText[2]), col = c(1, 
            2), lty = 1, cex = 0.8, horiz = TRUE, bty = "n")
    }
    plot(air_temperature ~ DOY, data = data1, xlim = limList$DOY, 
        ylim = limList$Tair, col = 1, xlab = "", ylab = "", xaxt = "n", 
        yaxt = "n", ...)
    if (!is.null(data2)) {
        points(air_temperature ~ DOY, data = data2, xlim = c(201, 
            238), col = 2, ...)
    }
    axis(1, at = seq(limList$DOY[1], limList$DOY[2], step), labels = FALSE)
    axis(2, labels = FALSE)
    axis(4, labels = TRUE)
    mtext(4, text = expression(paste("T"[a] * " ("^o ~ "C)")), 
        cex = 0.8, outer = FALSE, line = 3.5)
    abline(h = 0, lty = 3, lwd = 1.5)
    if (!is.null(legendText)) {
        legend("topleft", c(legendText[1], legendText[2]), col = c(1, 
            2), lty = 1, cex = 0.8, horiz = TRUE, bty = "n")
    }
    plot(VPD ~ DOY, data = data1, xlim = limList$DOY, ylim = limList$VPD, 
        col = 1, xlab = "", ylab = "VPD (kPa)", xaxt = "n", ...)
    if (!is.null(data2)) {
        points(VPD ~ DOY, data = data2, xlim = c(201, 238), col = 2, 
            ...)
    }
    axis(1, at = seq(limList$DOY[1], limList$DOY[2], step), labels = FALSE)
    axis(4, labels = FALSE)
    abline(h = 0, lty = 3, lwd = 1.5)
    if (!is.null(legendText)) {
        legend("topleft", c(legendText[1], legendText[2]), col = c(1, 
            2), lty = 1, cex = 0.8, horiz = TRUE, bty = "n")
    }
    if (length(grep("Rn_1_1_1", names(data1))) == 0) {
        print("Net radiation (Rn_1_1_1) does not exist in the data frame and will not be plotted")
    }
    else {
        plot(Rn_1_1_1 ~ DOY, data = data1, xlim = limList$DOY, 
            ylim = limList$Rn, col = 1, xlab = "", ylab = "", 
            xaxt = "n", yaxt = "n", ...)
        if (!is.null(data2)) {
            points(Rn_1_1_1 ~ DOY, data = data2, xlim = c(201, 
                238), col = 2, ...)
        }
        axis(1, at = seq(limList$DOY[1], limList$DOY[2], step), 
            labels = FALSE)
        axis(2, labels = FALSE)
        axis(4, labels = TRUE)
        mtext(4, text = expression(paste("Rn (W m"^"-2", ")")), 
            cex = 0.8, outer = FALSE, line = 3.5)
        abline(h = 0, lty = 3, lwd = 1.5)
        if (!is.null(legendText)) {
            legend("topleft", c(legendText[1], legendText[2]), 
                col = c(1, 2), lty = 1, cex = 0.8, horiz = TRUE, 
                bty = "n")
        }
    }
    if (length(grep("Rn_1_1_1", names(data1))) == 0) {
        plot(wind_speed ~ DOY, data = data1, xlim = limList$DOY, 
            ylim = limList$WS, col = 1, xlab = "", ylab = "", 
            xaxt = "n", yaxt = "n", ...)
        if (!is.null(data2)) {
            points(wind_speed ~ DOY, data = data2, xlim = c(201, 
                238), col = 2, ...)
        }
        axis(1, at = seq(limList$DOY[1], limList$DOY[2], step), 
            labels = TRUE)
        axis(2, labels = FALSE)
        axis(4, labels = TRUE)
        mtext(4, text = expression(paste("u (m s"^-1 ~ ")")), 
            cex = 0.8, outer = FALSE, line = 3.5)
        mtext("Day of Year", side = 1, outer = TRUE, cex = 0.8, 
            line = 2.5)
        abline(h = 0, lty = 3, lwd = 1.5)
        if (!is.null(legendText)) {
            legend("topleft", c(legendText[1], legendText[2]), 
                col = c(1, 2), lty = 1, cex = 0.8, horiz = TRUE, 
                bty = "n")
        }
    }
    else {
        plot(wind_speed ~ DOY, data = data1, xlim = limList$DOY, 
            ylim = limList$WS, col = 1, xlab = "", ylab = "u (m s"^-1 ~ 
                ")", xaxt = "n", ...)
        if (!is.null(data2)) {
            points(wind_speed ~ DOY, data = data2, xlim = c(201, 
                238), col = 2, ...)
        }
        axis(1, at = seq(limList$DOY[1], limList$DOY[2], step), 
            labels = TRUE)
        axis(4, labels = FALSE)
        mtext("Day of Year", side = 1, outer = TRUE, cex = 0.8, 
            line = 2.5)
        abline(h = 0, lty = 3, lwd = 1.5)
        if (!is.null(legendText)) {
            legend("topleft", c(legendText[1], legendText[2]), 
                col = c(1, 2), lty = 1, cex = 0.8, horiz = TRUE, 
                bty = "n")
        }
    }
}
