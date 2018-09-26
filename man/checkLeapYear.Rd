\name{checkLeapYear}
\alias{checkLeapYear}
\title{
  Check if it is a leap year
}
\description{
  It checks if the year is leap and returns a TRUE/FALSE argument.
  It requires the \emph{lubridate} package.
}
\usage{
checkLeapYear(ts)
}
\arguments{
  \item{ts}{
    The timestamp as a POSIXt obejct
  }
}
\details{
  The timestamp should be a POSIXt object which includes the year.
}
\author{
  Georgios Xenakis
}
%% \examples{
%% ##Load the data
%% data(fluxes)

%% ## Create the time stamp
%% fluxes=checkLeapYear(fluxes)
%% }                                       
