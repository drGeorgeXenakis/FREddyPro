\name{plotQC}
\alias{plotQC}
\title{
  Plot using QC flags
}
\description{
  Plot fluxes using different colour and points for each QC.
}
\usage{
plotQC(data, var, qc_var=NULL, xvar, lines = FALSE, legendSide = NULL,
ylab = var, xlab = "DOY", col = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
pch = c(16, 17, 18, 19, 20, 21, 22, 23, 24), \dots)
}
\arguments{
  \item{data}{
    The EddyPro full output data frame
  }
  \item{var}{
    Character. The name of the variable to plot. Default variable is CO2 flux.
  }
  \item{qc_var}{
    Character. The name of the qc variable to plot.
  }
  \item{xvar}{
    Character. The name of the X variable to plot. Default variable is DOY.
  }
  \item{lines}{
    Logical. If TRUE a line will be drawn between each point
  }
  \item{legendSide}{
    Side where to place the legend of the plot. Options include
    'topright', 'topleft', 'bottomright', 'bottomleft', 'center',
    'bottom', 'left', 'right', 'top'. For more details see \emph{legend}
    help file
  }
  \item{ylab}{
    y axis label
  }
  \item{xlab}{
    x axis label
  }
  \item{col}{
    Colour of the points. This can either be a single value or a vector
    for each of the flag
  }
  \item{pch}{
    Integer or single character. The type of points
  } 
  \item{\dots}{
    Any extra plotting arguments
  }
}
\author{
  Georgios Xenakis
}
\note{
  The variable should have a QC flag. Currently the function works on the
  three flag system i.e., 0, 1 and 2. 
}
\examples{
## Close any previously open graphic devices
graphics.off()

## Load the data
data(fluxes)

## Clean fluxes
fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,timesList=3,distCor=TRUE,
                    thresholdList=list(H=c(-100,1000),LE=c(-100,1000)))

## Plot CO2 fluxes based on the QC flag
plotQC(fluxes,"co2_flux",legendSide='bottomleft',xlab="Day of year",
        lines=TRUE,lty=2,ylab="F"[c]~" (umol/m"^2~"/s)",col=c(1,1,1),pch=c(1,2,8),
        xlim=c(150,155),xaxt='n',main='QC flags')

## Draw a new X-axis
axis(1,at=seq(150,155,0.5),labels=TRUE)

## A horizontal line
abline(h=0,lty=2)
}
