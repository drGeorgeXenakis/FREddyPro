\name{calculateFootprint}
\alias{calculateFootprint}
\title{
  Minimum argument footprint calculation
}
\description{
  A wrapper function for quick calculation of the average footprint for
  a specific period of time with minimum required arguments
}
\usage{
calculateFootprint(df, displacement, stability = NULL, fetch = 500,
grid = 200, height, lowerDay = NULL, upperDay = NULL)
}
\arguments{
  \item{df}{
    The data frame to use
  }
  \item{displacement}{
    Displacement height in meters.
  }
  \item{stability}{
    Integer used to mask the data for stability. 1=Unstable, 2=neutral,
    3=stable conditions.
  }
    \item{fetch}{
    The upwind distance over which you want the calculation domain to extend. 
    Note that the footprint will be normalized to that domain so footprints that
    extend well beyond the domain will be overrepresented in
  }
  \item{grid}{
    The total calculation grid size. The default is 200 which would mean
    the footprint would be represented in a 200x200 array corresponding
    to dimensions of two times the fetch on each side of the grid.
  }
  \item{height}{
    The z-d height of the flux system
  }
  \item{lowerDay}{
    The lower limit of the period to calculate the footprint
  }
  \item{upperDay}{
    The upper limit of the period to calculate the footprint
  }
}
\value{
  Returns a list object of a footprint similar to that of the function Average
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

## You can also use the plot.footprint function of FREddyPro to plot the result
plotFootprint(ftp)
}
