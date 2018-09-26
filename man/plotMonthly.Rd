\name{plotMonthly}
\alias{plotMonthly}
\title{
  Plot monthly averages
}
\description{
  A function to plot diurnal patterns for each individual month.
}
\usage{
plotMonthly(data, var, legend = FALSE, legendSide = NULL, type = "o",
col = 1, lty = 1, yaxt.out = NULL, yaxt.in = NULL, xaxt.out = NULL,
xaxt.in = NULL, axis1.in = FALSE, at1.in = NULL, axis2.in = FALSE,
at2.in = NULL, axis1.out = FALSE, at1.out = NULL, axis2.out = FALSE,
at2.out = NULL, ...)
}
\arguments{
  \item{data}{
    The data frame.
  }
  \item{var}{
    The variable to plot. The default is CO2 flux.
  }
  \item{legend}{
    Logical. If TRUE a legend will be plotted for each subplot.
  }
  \item{legendSide}{
    The side at which to plot the legend.
  }
  \item{type}{
    The type of plot. The default is a point-line.
  }
  \item{col}{
    The color of the line/points.
  }
  \item{lty}{
    The type of line.
  }
  \item{yaxt.out}{
    Drawing the outside y ticks and labels. If 'n' then they are not drawn.
  }
  \item{yaxt.in}{
    Drawing the inside y ticks and labels. If 'n' then they are not drawn.
  }
  \item{xaxt.out}{
    Drawing the outside x ticks and labels. If 'n' then they are not drawn.
  }
  \item{xaxt.in}{
    Drawing the inside x ticks and labels. If 'n' then they are not drawn.
  }
  \item{axis1.in}{
    Drawing the inside x ticks and labels. If 'n' then they are not drawn.
  }
  \item{at1.in}{
    Variable to show where to draw the new ticks for the bottom axis of
    the inside plots.
  }
  \item{axis2.in}{
    Logical. If TRUE the left axis will be drawn for the inside plots.
  }
  \item{at2.in}{
    Variable to show where to draw the new ticks for the left axis of
    the outside plots.
  }
  \item{axis1.out}{
    Logical. If TRUE the bottom axis will be drawn for the outside plots.
  }
  \item{at1.out}{
    Variable to show where to draw the new ticks for the bottom axis of
    the outside plots.
  }
  \item{axis2.out}{
    Logical. If TRUE the left axis will be drawn for the outside plots.
  }
  \item{at2.out}{
    Variable to show where to draw the new ticks for the left axis of
    the outside plots.
  }
  \item{\dots}{
    Further graphical parameters.
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
fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,distCor=TRUE,timesList=3,
thresholdList=list(H=c(-100,1000),LE=c(-100,1000)))	

## Plot monthly
plotMonthly(fluxes,"co2_flux",legend=TRUE,legendSide='bottomleft',type='o',
lty=2,ylim=c(-50,20),col=1,yaxt.in='n',yaxt.out='n',xaxt.in='n',xaxt.out='n',
at2.out=seq(-50,10,10),axis2.out=TRUE,at1.out=seq(0,23,2),axis1.out=TRUE,
at1.in=seq(0,23,2),axis1.in=TRUE)

## Put some text as title and axis labels
mtext(side=3,"Harwood forest",outer=TRUE,line=2,cex=1.2)
mtext(side=2,"Fc (umol m"^-2~" s"^-1~")",outer=TRUE,line=2,cex=1.2)
mtext(side=1,"Hour",outer=TRUE,line=3,cex=1.2)
}

