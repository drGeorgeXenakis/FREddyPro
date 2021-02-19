.__global__ <-
"."
polar2cart <-
function (x, y, dist, bearing, as.deg = FALSE) 
{
    if (as.deg) {
        bearing = bearing * pi/180
    }
    newx <- x + dist * sin(bearing)
    newy <- y + dist * cos(bearing)
    return(list(x = newx, y = newy))
}
