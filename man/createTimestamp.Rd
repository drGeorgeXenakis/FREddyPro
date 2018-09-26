\name{createTimestamp}
\alias{createTimestamp}
\title{
  Create a time stamp
}
\description{
  It creates a new timestam colume using the date and time
  output of EddyPro. It then converts the column to a POSIXct
  class and extracts to separate columns the year, month, day and
  hour. It requires the \emph{lubridate} package.
}
\usage{
createTimestamp(data, timestamp = NULL, timeZone = "GMT")
}
\arguments{
  \item{data}{
    The data frame.
  }
  \item{timestamp}{
    A character giving the name of the timestamp column .
  }
  \item{timeZone}{
    A character giving the time zone. Default is GMT.
  }
}
\details{
  For the function to work properly, two columns need to exist. The data
  and time, each one with a specific format. The date column should be
  formated as \strong{YYYY-MM-DD} and the time as \strong{HH:MM}. The
  functions can be used with an existing timestamp column too and that
  should be formated as \strong{YYYY-MM-DD HH:MM:SS}.

}
\author{
  Georgios Xenakis
}
\examples{
##Load the data
data(fluxes)

## Create the time stamp
fluxes=createTimestamp(fluxes)
}
