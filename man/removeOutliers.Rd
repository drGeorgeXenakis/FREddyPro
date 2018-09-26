\name{removeOutliers}
\alias{removeOutliers}
\title{
  Remove outliers
}
\description{
  Function to remove statistical outliers
}
\usage{
removeOutliers(x, na.rm = TRUE, ...)
}
\arguments{
  \item{x}{
    Variable to remove outliers
  }
  \item{na.rm}{
    Logical option for NA values. If TRUE NA value will be ignored.
  }
  \item{\dots}{
    Further arguments
  }
}
\author{
  Georgios Xenakis
}
\examples{
## Load the data
data(fluxes)

## Remove outliers for evapotranspiration
fluxes$E<-removeOutliers(fluxes$E)
}
