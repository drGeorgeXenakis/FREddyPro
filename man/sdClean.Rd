\name{sdClean}
\alias{sdClean}
\title{
  Clean using standard deviation
}
\description{
  Functions which splits the fluxes to positive and negative, estimates
  mean and standard deviation and removes values which are greater than
  a predefined times the standard deviation
}
\usage{
sdClean(var, p)
}
\arguments{
  \item{var}{
    The variable to clean
  }
  \item{p}{
    A number giving the times of the standard deviation
  }
}
\author{
  Georgios Xenakis
}
\examples{
## Load the data
data(fluxes)

## Clean CO2 fluxes using 3 times the SD for negative and positive
## values separately
fluxes$co2_flux<-sdClean(fluxes$co2_flux,3)
}
