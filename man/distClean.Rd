\name{distClean}
\alias{distClean}
\title{
  Distributional cleaning
}
\description{
  Function to clean and de-spike gas flux data based on a distribution
  produced for each half hour and removing values lower and higher than
  the 5th and 95th percentile respectively
}
\usage{
distClean(var, hour, df)
}
\arguments{
  \item{var}{
    The variable to clean/de-spike
  }
  \item{hour}{
    The hour variable created by the createTimestamp function
  }
  \item{df}{
    The data frame
  }
}
\author{
  Georgios Xenakis
}
\examples{
## Load the data
data(fluxes)

## Clean data
fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,timesList=3,distCor=TRUE,
                   thresholdList=list(H=c(-100,1000),LE=c(-100,1000)))	

## Create timestamp
fluxes<-createTimestamp(fluxes)

## Find which rows have positive and which negative CO2 flux values 
positive<-which(fluxes$co2_flux>0)
negative<-which(fluxes$co2_flux<0)

## Clean data for positive and negative separately
fluxes$co2_flux[positive]<-distClean(fluxes$co2_flux[positive],
                                     fluxes$hour[positive],fluxes[positive,])
fluxes$co2_flux[negative]<-distClean(fluxes$co2_flux[negative],
                                     fluxes$hour[negative],fluxes[negative,])
}
