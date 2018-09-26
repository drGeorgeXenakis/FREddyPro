\name{ustarThreshold}
\alias{ustarThreshold}
\title{
  Find the u* threshold
}
\description{
  Function implementing the methodology by Papale et al. 2006
}
\usage{
ustarThreshold(data, sunset = 19, sunrise = 6)
}
\arguments{
  \item{data}{
    The data frame
  }
  \item{sunset}{
    Time of sunset as a real number (0-23)
  }
  \item{sunrise}{
    Time of sunrise as a real number (0-23)
  }
}
\details{
  Air temperature is split into 7 classes and u* within each class is
  split into 20 classes (see Papale et al. 2006). Sunrise and sunset
  time is used to define nightime data
}
\references{
  Papale D, Reichstein M, Aubinet M, Canfora E, Bernhofer C, Kutsch W,
  Longdoz B, Rambal S, Valentini R, Vesala T & et al. (2006)
  Towards a standardized processing of Net Ecosystem Exchange measured
  with eddy covariance technique: algorithms and uncertainty estimation. 
  Biogeosciences, Copernicus GmbH, 3, 571 - 583 

}
\author{
  Georgios Xenakis
}
\examples{
## Load the data
data(fluxes)

## Calculate U* and print on screen
ustar=ustarThreshold(fluxes,sunset=19,sunrise=6)
print(ustar)

}
