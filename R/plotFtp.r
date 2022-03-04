.__global__ <-
"."
plotFtp <-
function (ftp, tl = NULL, xy = 500, j = 200) 
{
    l <- ggplot(ftp, aes(x, y))
    l <- l + geom_raster(aes(fill = cumz), interpolate = T) + 
        scale_fill_gradientn(colours = rainbow(7), name = "Cumulative\nfootprint [%]", 
            limits = c(0, 100), breaks = seq(0, 100, 20), na.value = "transparent") + 
        theme_bw() + scale_x_continuous(limits = c(-xy, xy), 
        breaks = seq(-xy, xy, j)) + scale_y_continuous(limits = c(-xy, 
        xy), breaks = seq(-xy, xy, j)) + labs(x = "Distance from tower [m]", 
        y = "Distance from tower [m]", title = tl) + theme(panel.background = element_blank(), 
        plot.title = element_text(colour = "black", size = 12, 
            hjust = 0.5), axis.title.x = element_text(colour = "black", 
            size = 10), axis.title.y = element_text(colour = "black", 
            size = 10), axis.text.x = element_text(colour = "black", 
            size = 10), axis.text.y = element_text(colour = "black", 
            size = 10), axis.ticks.x = element_line(colour = "black"), 
        axis.ticks.y = element_line(colour = "black"), panel.grid.major.x = element_line(colour = "grey"), 
        panel.grid.major.y = element_line(colour = "grey"))
    return(l)
}
