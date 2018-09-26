\name{cleanVarG}
\alias{cleanVarG}
\title{
  Clean a variable above a threshold
}
\description{
  Remove values based if they are greter than the given thresholds
}
\usage{
cleanVarG(data, x, y = NULL, greaterThan = NULL)
}
\arguments{
  \item{data}{
    The data frame
  }
  \item{x}{
    A character giving the name of the variable to clean
  }
  \item{y}{
    A second character giving the name of the variable to clean based on
    the selection of x variable
  }  
  \item{greaterThan}{
    A number giving the "greater than" threshold for removing values from
    the variable
  }
}
\author{
Georgios Xenakis
}
\examples{
## Load the data
data(fluxes)

## Clean a variable
fluxes<-cleanVarG(data=fluxes,x="H",greaterThan=1500)
fluxes<-cleanVarG(data=fluxes,x="DOY",y="H",greaterThan=175)
}
