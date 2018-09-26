\name{plotDaily}
\alias{plotDaily}
\title{
  Plot average daily patterns
}
\description{
  Calculating and plotting the daily pattern of any variable with the
  option to use mean or both mean and median. There is also an option to
  plot quantiles.
}
\usage{
plotDaily(x, day.x, y = NULL, day.y = NULL, median = FALSE,
quantiles = FALSE, probs = c(0.05, 0.95), legend = FALSE, legendSide =
NULL, legendText = NULL, type = "l", lty = c(1, 4), col = c(1, 2),
cex.legend = 1, horiz.legend = TRUE, ylab = "Mean and median", xlab =
"Day", \dots)
}
\arguments{
  \item{x}{
    The variable to plot
  }
  \item{day.x}{
    The day variable associated with the X variable
  }
  \item{y}{
    A second optional variable to plot.
  }
  \item{day.y}{
    The day variable associated with the Y variable, also optional.
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
  The function first aggregates the given variable daily and
  plots by default the mean. The median can also be plotted with the
  mean. There is an optional graphical output for quantiles. By
  default is the 5th and 95th but other quantiles can be used. There
  are two separate daily inputs to the functions to allow plotting
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

## Plot the daily patterns

plotDaily(x=fluxes$co2_flux,day.x=fluxes$yday,type='o',
median=TRUE,ylim=c(-30,20),lty=c(1,2),col=c(1,2),quantiles=TRUE,
legend=TRUE,legendSide='topleft',
lwd=2,ylab="Fc (umol m"^2~" s"^-1~")",main="Harwood forest",cex.legend=0.9,
horiz.legend=FALSE)
abline(h=0,lty=2)
}
