\name{percentQC}
\alias{percentQC}
\title{
  Percent of each QC
}
\description{
  Gives the percentage of a each QC flag for a variable and writes the
  result either on the standard output or a text file.
}
\usage{
  percentQC(data, var = "co2_flux", qc_var = "qc_co2_flux",
write = FALSE, dir = "./")
}
\arguments{
  \item{data}{
    A data frame
  }
  \item{var}{
    A string giving the name of the variable. 
  }
  \item{qc_var}{
    A string giving the name of the QC variable
  }
  \item{write}{
    Logical. If true the output is written on a text file rather the
    standard output. The name of the output file is standard and is
    \emph{percent_qc_flags.txt}. 
  }
  \item{dir}{
    The directory to write the output file. If none given then the
    working directory will be used.
  }
}
\details{
  By default the function will give the percentage of each QC flag for CO2
  flux. Other variables which have QC can be defined using the var
  option e.g., sensible heat, latent heat and momentum flux. The
  variable should have a QC flag for the function to work. If there is
  no QC flag associated with the variable, the result will be zero.
}
\value{
  Text with output percentages
}
\author{
  Georgios Xenakis
}
\note{
  Variable needs to have an associate QC flag. Function currently works
  for the three flag system i.e., 0, 1, 2.
}
\examples{
##Load the data
data(fluxes)

## Find the percent of each QC flag for CO2 before cleaning
percentQC(fluxes)

## Do the same for sensible heat
percentQC(fluxes,var="H")

## Clean data
fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,timesList=3,distCor=TRUE,
                   thresholdList=list(H=c(-100,1000),LE=c(-100,1000)))	

## Find the percent of each QC flag for CO2 after cleaning
percentQC(fluxes)

## Do the same for latent heat
percentQC(fluxes,var="H")
}
