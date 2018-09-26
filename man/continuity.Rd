\name{continuity}
\alias{continuity}
\title{
  Check timeseries continuity
}
\description{
  Function based on the timestamp to identify if there is a
  discontinuity in the timeseries
}
\usage{
continuity(data, timestamp = NULL, timediff = 30)
}
\arguments{
  \item{data}{
    The data frame   
  }
  \item{timestamp}{
    A string giving the name of the timestamp. 
  }
  \item{timediff}{
    
  }
}
\details{
  If no timestamp is given then the function assumes the standard
  EddyPro output (which originaly does not include a timestamp) and uses
  internally the createTimestamp function to create one.
}
\value{
  Returns a data frame
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

## Remove some line as an example
fluxes=fluxes[-c(832,833,834,835,840,953),]

## Create timestamp from date and time
continuity(fluxes)
}
