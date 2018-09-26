\name{midpoints}
\alias{midpoints}
\title{
  Mid-point of a class
}
\description{
  Calculate the mid-point of a class produced by the cut() command
}
\usage{
midpoints(x, dp = 2)
}
\arguments{
  \item{x}{
    The classes variable
  }
  \item{dp}{
    Integer giving the decimal digits of the output
  }
}
\examples{
## Load the data
data(fluxes)

## Split the wind data into 20 classes
fluxes$wind_class<-cut(fluxes$wind_dir,breaks=seq(0,365,20))

## Find the midpoint of each wind class
fluxes$wind_mid_points<-midpoints(fluxes$wind_class)
}
