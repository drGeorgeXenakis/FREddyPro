\name{plotFootprintMap}
\alias{plotFootprintMap}
\title{
  Plot a tower footprint on a map
}
\description{
  Function to plot a footprint model of a tower on a background map
  image from Google Maps.
}
\usage{
plotFootprintMap(footprint, X, Y, zoom, maptype = "hybrid", alpha = 0.35)
}
\arguments{
  \item{footprint}{
    The footprint object produced by the calculateFootprint, Calculate
    and Average functions
  }
  \item{X}{
    The X coordinate of the flux tower
  }
  \item{Y}{
    The Y coordinate of the flux tower
  }
  \item{zoom}{
    An integer giving the zoom for Google Maps
  }
  \item{maptype}{
    The type of Google map to draw. Options include "roadmap",
    "satellite", "hybrid", and "terrain".
  }
  \item{alpha}{
    Transparency factor for colours. 0 shows full transparency, 1 full non-transparency
  }
}
\author{
  Georgios Xenakis
}
\examples{
## Close any previously open graphic devices
graphics.off()

## Load data
data(fluxes)

## Clean and de-spike the fluxes
fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,timesList=3,distCor=TRUE,
thresholdList=list(H=c(-100,1000),LE=c(-100,1000)))

## Quick calculation of footprint
ftp=calculateFootprint(fluxes[39,],17.42,stability=NULL,fetch=500,grid=200,height=33)

## Plot the footprint
plotFootprintMap(ftp,X=398505,Y=591355,zoom=15,alpha=0.35)
}
