\name{percentNA}
\alias{percentNA}
\title{
  Percent of NA values
}
\description{
  Gives a percent of NA values for a specific column in a data frame
}
\usage{
  percentNA(var)
}
\arguments{
  \item{var}{
    The variable to calculate the percentage of NAs
  }
}
\author{
  Georgios Xenakis
}
\examples{
## Load the data
data(fluxes)

## Calculate percentage of NAs in the CO2 flux before cleaning data
percentNA(fluxes$co2_flux)

## Clean data
fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,timesList=3,distCor=TRUE,
                   thresholdList=list(H=c(-100,1000),LE=c(-100,1000)))	

## Calculate percentage of NAs after cleaning
percentNA(fluxes$co2_flux)
}
