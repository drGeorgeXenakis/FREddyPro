\name{rotateMatrix}
\alias{rotateMatrix}
\title{
  Rotate a matrix
}
\description{
  Rotates a matrix
}
\usage{
rotateMatrix(x)
}
\arguments{
  \item{x}{
    The matrix to rotate
  }
}
\author{
  Georgios Xenakis
}
\note{
  This is a function needed for internal purposes
}

\examples{
## Load the data
data(fluxes)

## Clean fluxes
fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,timesList=3,distCor=TRUE,
                   thresholdList=list(H=c(-100,1000),LE=c(-100,1000)))	

## Quick calculation of footprint
ftp=calculateFootprint(fluxes,17.42,stability=1,fetch=500,grid=200,height=33,
lowerDay=150,upperDay=151)

## The footprint as a matrix
m=ftp[[1]]

## Rotate matrix
rot.m=rotateMatrix(m)
}
