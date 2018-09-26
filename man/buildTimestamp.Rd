\name{buildTimestamp}
\alias{buildTimestamp}
\title{
  Build a timestamp
}
\description{
  Use a day of year to build a timestamp column with the format used by EddyPro
}
\usage{
buildTimestamp(data, doy, decimal.hour, origin)
}

\arguments{
  \item{data}{
    The data frame to use
  }
  \item{doy}{
    A string with the name for the day of year column
  }

  \item{decimal.hour}{
    A string with the name for the decimal hour
  }
  \item{origin}{
    A string with the origin time. Default value is "2016-01-01".
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

## Remove some line as an example
fluxes=fluxes[-c(832,833,834,835,840,953),]

## Create timestamp from date and time
fluxes=createTimestamp(fluxes)

## Now fill the gap timestmaps with NA's
fluxes=fillTimestamp(fluxes,flux=TRUE)

}
