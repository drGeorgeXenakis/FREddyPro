\name{exportFootprintPoints}
\alias{exportFootprintPoints}
\title{
  Write footprint as points
}
\description{
  Converts a two-dimensional footprint model to a data frame with X and
  Y coordinates
}
\usage{
exportFootprintPoints(ftp, xcoord, ycoord)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ftp}{
    The footprint model
}
  \item{xcoord}{
    The x coordinate of the tower
}
  \item{ycoord}{
    The y coordinate of the tower
  }
}
\author{
  Georgios Xenakis
}

\examples{
## Load the data
data(fluxes)

## Clean fluxes
fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,timesList=3,distCor=TRUE,
                   thresholdList=list(H=c(-100,1000),LE=c(-100,1000)))	

## Quick calculation of footprint
ftp=calculateFootprint(fluxes,17.42,stability=1,fetch=500,grid=200,height=33,
lowerDay=150,upperDay=151)

## Export the footprint as points
ftp_points=exportFootprintPoints(ftp, 0, 0)
}
