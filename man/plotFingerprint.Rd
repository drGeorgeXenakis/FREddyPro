\name{plotFingerprint}
\alias{plotFingerprint}
\title{
  Plot a fingerprint graph
}
\description{
  Plots a variable with day of year on the y-axis and the hour of the
  day on the x-axis. It produces the commonly used fingerprint plot.
}
\usage{
plotFingerprint(var, doy, hour, step = 2, xlab = "Hour",
ylab="Day or Year", ...)
}
\arguments{
  \item{var}{
    Variable to be plotted.
  }
  \item{doy}{
    The day of the year variable.
  }
  \item{hour}{
    The hour of the day variable.
  }
  \item{step}{
    The length between y-axis tics. Defaule is 2 days.
  }
  \item{xlab}{
    The label of the x-axis
  }
  \item{ylab}{
    The label of the y-axis
  }

  \item{\dots}{
    Further graphical parameters for the filled.contour() plot.
  }
}
\author{
  Georgios Xenakis
}
\examples{
## Close any previously open graphic devices
graphics.off()

## Load the data
data(fluxes)

## Clean the fluxes
fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,timesList=3,distCor=TRUE,
thresholdList=list(H=c(-100,1000),LE=c(-100,1000)))

## Plot the fingerprint plot
plotFingerprint(fluxes$co2_flux,fluxes$yday,fluxes$hour,step=1,main='CO'[2]~' flux',
key.title=title(main="umol m"^2~"s"^-1~"",adj=0.2))
}
