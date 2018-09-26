\name{removeDuplicates}
\alias{removeDuplicates}
\title{
  Remove duplicates
}
\description{
  The function looks at the timestamp of the data and identifies rows
  with same timestamp and removes one of the two
}
\usage{
removeDuplicates(data,timestamp="timestamp")
}
\arguments{
  \item{data}{
    The data frame to use.
  }
  \item{timestamp}{
    Character. The name of the timestamp colume. Default is set to "timestamp".
  }
}
\author{
  Georgios Xenakis
}
\examples{
## Load the data
data(fluxes)

## Remove duplicates
fluxes<-removeDuplicates(fluxes)
}
