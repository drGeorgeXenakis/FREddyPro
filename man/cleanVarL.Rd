\name{cleanVarL}
\alias{cleanVarL}
\title{
  Clean a variable below a threshold
}
\description{
  Remove values based if they are greter or less than the given thresholds
}
\usage{
cleanVarL(data, x, y = NULL, lessThan = NULL)
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
  \item{lessThan}{
    A number giving the "less than" threshold for removing values from
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
fluxes<-cleanVarL(data=fluxes,x="H",lessThan=-200)
fluxes<-cleanVarL(data=fluxes,x="DOY",y="H",lessThan=155)
}
