\name{addSiteName}
\alias{addSiteName}
\title{
  Create an extra column in a data frame with the name of the site
}
\description{
  Adds an extra column with the given string for each row in the data frame
}
\usage{
  addSiteName(data, name)
}
\arguments{
  \item{data}{
    The data frame
  }
  \item{name}{
    A string to write in the extra column, usually the name of the
    site e.g., \emph{"Harwood"}
  }
}
\details{
  The function is useful if you need an identifier when combine data
  with different characteristics
}
\value{
  Returns a column with character elements
}
\author{
  Georgios Xenakis
}
\examples{
##Load the data
data(fluxes)

## Clean data
fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,timesList=3,distCor=TRUE,
                   thresholdList=list(H=c(-100,1000),LE=c(-100,1000)))

## Add the name of the site
fluxes=addSiteName(fluxes,"Harwood")
}
