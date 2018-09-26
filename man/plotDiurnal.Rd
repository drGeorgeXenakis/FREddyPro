\name{plotDiurnal}
\alias{plotDiurnal}
\title{
  Plot average diurnal patterns
}
\description{
  Calculating and plotting the diurnal pattern of any variable with the
  option to use mean or both mean and median. There is also an option to
  plot quantiles.
}
\usage{
plotDiurnal(data, xVar, hourX, dataY = NULL, yVar = NULL, hourY = NULL,
median = FALSE, quantiles = FALSE, probs = c(0.05, 0.95),
stdev = FALSE, sterr = FALSE, legend = FALSE, legendSide = NULL,
legendText = NULL, type = "l", lty = c(1, 4), col = c(1, 2),
cex.legend = 1, horiz.legend = TRUE, ylab = "Mean", xlab = "Hour", \dots)
}
\arguments{
  \item{data}{
    The data frame to plot
  }
  \item{xVar}{
    The variable to plot
  }
  \item{hourX}{
    The hour variable associated with the X variable
  }
  \item{dataY}{
    A second data frame to plot (optional)
  }
  \item{yVar}{
    A second optional variable to plot.
  }
  \item{hourY}{
    The hour variable associated with the Y variable, also optional.
  }
  \item{median}{
   Logical. If TRUE the median is also plotted with the mean. 
  }
  \item{quantiles}{
    Logical. If TRUE the quantiles will be plotted as a colour band
  }
  \item{probs}{
    An array of the quantiles to use. The default is 0.05 and 0.95 (5th
    and 95th).
  }
  \item{stdev}{
    Logical. If True the standard deviation is plotted as a colour band
  }
  \item{sterr}{
    Logical. If True the standard error is plotted as a colour band
  }
  \item{legend}{
    Logical. If TRUE a legend will be plotted.
  }
  \item{legendSide}{
    The side of the plot to place the legend.
  }
  \item{legendText}{
    The text to use for legend.
  }
  \item{type}{
    The type of plot. The default is a line plot.
  }
  \item{lty}{
    The type of line when a line plot is used.
  }
  \item{col}{
    The colour to use for the lines or points. It can be a single values
    or an array.
  }
  \item{cex.legend}{
    The size of the legend labels.
  }
  \item{horiz.legend}{
    A logical parameter. If TRUE the legend is will be placed horizontally. 
  }
  \item{ylab}{
    The y-axis label.
  }
  \item{xlab}{
    The x-axis label.
  }
  \item{\dots}{
    Further graphical options.
  }
}
\details{
  The function first aggregates the given variable to half hourly and
  plots by default the mean. The median can also be plotted with the
  mean. There is an optional graphical output for quantiles. By
  default is the 5th and 95th but other quantiles can be used. There
  are two separate hourly inputs to the functions to allow plotting
  variables from different data frames.
}
\author{
  Georgios Xenakis
}
\examples{
## Load the data
data(fluxes)

## Clean and despike the fluxes
fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,distCor=TRUE,timesList=3,
thresholdList=list(H=c(-100,1000),LE=c(-100,1000)))	

## Create timestamp
fluxes=createTimestamp(fluxes)

## Plot the diurnal patterns
plotDiurnal(fluxes, xVar="H", hourX="hour",yVar="LE",hourY="hour",
median=TRUE,stdev=TRUE,ylim=c(-100,400),lty=c(1,2,3,4),col=c(1,2,3,4),
legend=TRUE,legendSide='topleft',legendText=c("sensible heat","latent heat"),
lwd=2,ylab="Heat (W/m"^2~")",main="Harwood forest",cex.legend=0.9,
horiz.legend=FALSE)

abline(h=0,lty=2)
}
