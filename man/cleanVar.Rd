\name{cleanVar}
\alias{cleanVar}
\title{
  Clean a variable
}
\description{
  Remove values based if they are greter or less than the given thresholds
}
\usage{
cleanVar(x, data, lessThan = NULL, greaterThan = NULL)
}
\arguments{
  \item{x}{
    A character giving the name of the variable to clean
  }
  \item{data}{
    The data frame
  }
  \item{lessThan}{
    A number giving the "less than" threshold for removing values from
    the variable
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
fluxes<-cleanVar("H",fluxes,lessThan=-200,greaterThan=1500)
}
