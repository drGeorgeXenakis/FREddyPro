.__global__ <-
"."
plotFootprintMap <-
function (footprint, X, Y, zoom, maptype = "hybrid", alpha = 0.35) 
{
    x = t(apply(footprint[[2]], 2, rev))
    y = t(apply(footprint[[3]], 2, rev))
    z = t(apply(footprint[[1]], 2, rev))
    df = data.frame(x = numeric(length(as.vector(x))), y = numeric(length(as.vector(y))), 
        z = numeric(length(as.vector(z))))
    df$x = as.vector(x)
    df$y = as.vector(y)
    df$z = as.vector(z)
    dfosgb36 <- df
    dfosgb36$x <- dfosgb36[, 1] + X
    dfosgb36$y <- dfosgb36[, 2] + Y
    r = rasterFromXYZ(dfosgb36)
    crs(r) <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
    micolor <- rev(rainbow(12, alpha = alpha))
    transp <- rainbow(12, alpha = 0)
    micolor[1:1] <- transp[1]
    mymap <- gmap(x = r, type = maptype, zoom = zoom)
    rGM <- projectRaster(from = r, crs = CRS("+init=epsg:3857"))
    rGM[rGM < 0] <- 0
    rGM[rGM > 1] <- 1
    plot(mymap)
    plot(rGM, add = T, legend = F, col = micolor)
}
