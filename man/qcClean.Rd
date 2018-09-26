\name{qcClean}
\alias{qcClean}
\title{
  Clean using QC flag
}
\description{
  Function cleaning data based on QC flags. 
}
\usage{
qcClean(var, qcVar, qc)
}

\arguments{
  \item{var}{
    The variable to clean.
  }
  \item{qcVar}{
    The associated QC flags variable.
  }
  \item{qc}{
    An integer indicating the QC flags to remove. It can be a single
    value or an array of values. Default value is 2.
  }
}
\author{
  Georgios Xenakis
}
\examples{
## Load the data
data(fluxes)

## Remove QC flags 1 and 2 for CO2
fluxes$co2_flux<-qcClean(fluxes$co2_flux,fluxes$qc_co2_flux,c(1,2))
}
