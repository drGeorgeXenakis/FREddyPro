\name{plotSpectra}
\alias{plotSpectra}
\title{
  Plot spectra
}
\description{
  Function to plot the ensemble spectra output from EddyPro.
}
\usage{
plotSpectra(data, gas, xlab = "Frequency [Hz]", ylab = "Spectrum", avgT
= FALSE, predicted = FALSE, type = c("o", "o", "o"), col = c(1, 2, 4),
pch=c(1,1,1), na.value = "NaN", ...)
}
\arguments{
  \item{data}{
    The data frame.
  }
  \item{gas}{
    A character giving the name of the gas which spectra will be plotted.
  }
  \item{xlab}{
    The label for the x axis.
  }
  \item{ylab}{
    The label for the y axis.
  }
  \item{avgT}{
    Logical. If TRUE the avgT will be plotted.
  }
  \item{predicted}{
    Logical. If TRUE the predicted spectra will be plotted.
  }
  \item{type}{
    The type of plot. Default is point-line. It can be a single value or
    an array.
  }
  \item{col}{
    The color of the points/lines. It can be a single value or an array.
  }
  \item{pch}{
    The type of points to use. It can be a single value or an array.
  }
  \item{na.value}{
    The number/string to use for identifying NA values.
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
data(spectra)

## Plot the spectra
plotSpectra(spectra,"co2",predict=TRUE,avgT=TRUE,type=c('p','l','l'),pch=c(16,-1,-1))
}

