\name{readFile}
\alias{readFile}
\title{
  Read a file
}
\description{
  A wrapper function for read.table for easy reading files and ignoring a
  given number of header rows
}
\usage{
readFile(dataFile, nSkip = 0, nSkipNames = 0, header = FALSE, reverse =
FALSE, sep = ",", na = "NaN", ...)
}
\arguments{
  \item{dataFile}{
    A character with the name of the file to read. It can include a full
    length path.
  }
  \item{nSkip}{
    The number of rows to skip to read the data
  }
  \item{nSkipNames}{
    The number of rows to skip to read the column names
  }
  \item{header}{
    Logical, for indicating if the file has a header or not. Default is TRUE.
  }
  \item{reverse}{
    Logical. If true then first reads a header and then skips a number
    or rows to read the data. Works in conjunction with nSkip option
  }
  \item{sep}{
    The field separator. Default is comma delimited
  }
  \item{na}{
    The value used in the file to represent NAs (e.g., -9999). The
    default is \emph{NaN}
  }
  \item{...}{
    Further arguments
  }
}
\details{
  It read a file by skiping first a number of rows to read the data and
  then reads the file again skiping a second number of rows to
  read the row with the variable names. 
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
harwood=readFile('harwood.csv',3,1,na='NaN')
}
