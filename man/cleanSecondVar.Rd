\name{cleanSecondVar}
\alias{cleanSecondVar}
\title{
  Clean a second variable
}
\description{
  It cleans a second variable giving another variable so the NA
  correspond row by row
}
\usage{
cleanSecondVar(x, y, data)
}
\arguments{
  \item{x}{
    A character giving the name of the first variable
  }
  \item{y}{
    A charcter giving the name of the second variable for which we want
    values to be removed
  }
  \item{data}{
    The data frame
  }
}
\author{
  Georgios Xenakis
}
\examples{
## Load the data
data(fluxes)

## Clean a second variable based on the first
fluxes<-cleanSecondVar(x="H",y="qc_H",data=fluxes)
}
