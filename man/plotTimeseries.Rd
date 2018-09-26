\name{plotTimeseries}
\alias{plotTimeseries}
\title{
  Plot a time series plot
}
\description{
  A function to plot some of the most commonly and important variables
  given in a standard EddyPro output file as a time series plot. The
  variables which will be plotted include 1) CO2 flux, sensible heat,
  latent heat, air temperature, vapour pressure deficit, and wind
  speed. Net radiation is optional.
}
\usage{
plotTimeseries(data1, limList = NULL, data2 = NULL, step = 1,
legendText = NULL, ...)
}
\arguments{
  \item{data1}{
    The data frame to use for plotting.
}
  \item{limList}{
    A list giving the ylim for the plotted variables. If a variable is
    missing from the list, ylim is derived from the range of the
    data. Not all variables are necessary to appear in the list.
}
  \item{data2}{
    A second data frame to use for plotting and compare with the
    first. This is optional.
}
  \item{step}{
    The length between tics in the x-axis. Default is 1 day
}
  \item{legendText}{
    The legend to appear in each subplot. This is usually necessary when
    two data frames are used which might be from different sites or treatments.
}
  \item{\dots}{
    Further graphical paramters.
  }
}
\note{
  Net radiation is an optional output and will only be plotted if it
  exists in the data frame. If not, will be ignored. The name of the net
  radiation variable should be the same with the one used by EddyPro's
  biomet format i.e., Rn_1_1_1. In fact, all variables should have the
  original naming otherwise it will give an error.
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
fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,distCor=TRUE,timesList=3,
thresholdList=list(H=c(-100,1000),LE=c(-100,1000)))	

## Plot all major data in as a timeseries.
plotTimeseries(fluxes,limList=list(DOY=c(146,183)),step=2,type='o',
cex=0.6,pch=1,lwd=0.5,lty=2)
}
