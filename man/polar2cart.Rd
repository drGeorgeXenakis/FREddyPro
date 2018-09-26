\name{polar2cart}
\alias{polar2cart}
\title{
  Polar to cartesian
}
\description{
  Converts polar co-ordinates to the cartesian system.
}
\usage{
polar2cart(x, y, dist, bearing, as.deg = FALSE)
}
\arguments{
  \item{x}{
    The X cartesian co-ordinate of the point of origin.
  }
  \item{y}{
    The y cartesian co-ordinate of the point of origin.
  }
  \item{dist}{
    The distance (m) of each point from the point of origin.
  }
  \item{bearing}{
    The polar bearing of each point to convert.
  }
  \item{as.deg}{
    Logical. If true the polar bearing is in degrees.
  }
}
\examples{
## Close any previously open graphic devices
graphics.off()

## Load the data
data(fluxes)

## Set the point of origin
x=0;y=0

## Use wind direction, distance and distance from the tower to convert
## to cartesian co-ordinates
pol<-polar2cart(x,y,fluxes$x_90.,fluxes$wind_dir,as.deg=TRUE)

## Plot the distance from the tower
plot(pol,xlab='X (m)',ylab='Y (m)',pch=16,cex=0.6)

## Add also the tower point
points(0,0,col=2,pch=16,cex=1.2)
}
