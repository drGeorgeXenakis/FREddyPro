\name{calculatePercentFootprint}
\alias{calculatePercentFootprint}
\title{
  Calculate Percent Footprint
}
\description{
  Function which calculates the accumulated percentage footpring contribution
}
\usage{
  calculatePercentFootprint(ftp, percent = 99.99)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ftp}{
    A footprint object
}
  \item{percent}{
    The cut-off percentage point for visual representation
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

## Calculate the footprint as an accumulated percentage and mask it for
## up to 60\%
ftpP=calculatePercentFootprint(ftp,percent=60)

## You can also use the plot.footprint function of FREddyPro to plot the result
plotFootprint(ftpP)
}
