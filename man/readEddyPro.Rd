\name{readEddyPro}
\alias{readEddyPro}
\title{
  Read EddyPro full output
}
\description{
  A function to read the standard full output produced by EddyPro.
}
\usage{
readEddyPro(dataFile, na = "NaN", sep = ",", ...)
}
\arguments{
  \item{dataFile}{
    A character with the name of the output CSV file. String can also
    include full path.
  }
  \item{na}{
    The value used in the file to represent NAs (e.g., -9999). The
    default is "NaN"
  }
  \item{sep}{
    The field separator. Default is comma delimited
  }
  \item{...}{
    Further arguments
  }  
}
\details{
  The EddyPro full output file has a standardise format with three
  header rows. The function calls the readFile function of the package
  with this standard number of headers. 
}
\value{
  Returns a data frame
}
\author{
  Georgios Xenakis
}
\examples{
## Load the data
data(harwood)

## Write the data as csv
write.table(harwood,file="harwood.csv",sep=",",quote=FALSE,row.names=FALSE)

## Now read the file as a new data frame
harwood=readEddyPro('harwood.csv')
}
