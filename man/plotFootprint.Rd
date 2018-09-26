\name{plotFootprint}
\alias{plotFootprint}
\title{
  Plot a tower footprint
}
\description{
  Plots the output of a Kormann and  Meixner footprint model.
}
\usage{
plotFootprint(footprint, main.title = NULL, key.title =
NULL, color.palette = NULL, ...)
}
\arguments{
  \item{footprint}{
    The footprint output produced by either \emph{Calculate} or
    \emph{Average} function
  }
  \item{main.title}{
    The title of the plot. Default is "Footprint"
  }
  \item{key.title}{
    The title over the key bar. Default is "Probability"
  }
  \item{color.palette}{
    The color palette to use.
  }
  \item{\dots}{
    Further graphical arguments for the filled.contour() function
  }
}
\author{
  Georgios Xenakis
}
\examples{
## Close any previously open graphic devices
graphics.off()

## Load data
data(fluxes)

## Clean and de-spike the fluxes
fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,timesList=3,distCor=TRUE,
thresholdList=list(H=c(-100,1000),LE=c(-100,1000)))

## Use only non-NA data
fluxes_noNA<-fluxes[which(!is.na(fluxes$L)),]

## Displacement height
d=17.42 ## Displacement height

## Calculate input variables for the footprint model
fluxes_noNA$z=(fluxes_noNA$X.z.d..L*fluxes_noNA$L)+d
fluxes_noNA$zol=fluxes_noNA$z/fluxes_noNA$L

## Calculate the footprint for day 853
footprint=Calculate(fetch=500,height=33,grid=200,fluxes_noNA$wind_speed[853],
fluxes_noNA$wind_dir[853],fluxes_noNA$u.[853],fluxes_noNA$zol[853],
sqrt(fluxes_noNA$u_var[853]))

## Plot the footprint
plotFootprint(footprint)
}
