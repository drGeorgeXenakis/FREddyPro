\name{plotWindrose}
\alias{plotWindrose}
\title{
  Plot a wind rose 
}
\description{
  Create a wind rose plot showing the direction and speed of wind
}
\usage{
plotWindrose(data, spd, dir, spdres = 2, dirres = 30, spdmin = 2,
spdmax = 20, spdseq = NULL, palette = "YlGnBu", countmax = NA, labels =
waiver(), title = NULL, xlab = "Direction", ylab = "Count", fill.name =
"Wind speed (m/s)", debug = 0)
}
\arguments{
  \item{data}{
    The data frame.
  }
  \item{spd}{
    Wind speed (m/s).
  }
  \item{dir}{
    Wind direction (degrees).
  }
  \item{spdres}{
    Wind speed resolution (m/s).
  }
  \item{dirres}{
    Wind direction resolution (degrees).
  }
  \item{spdmin}{
    Minimum wind speed (m/s).
  }
  \item{spdmax}{
    Maximum wind speed (m/s).
  }
  \item{spdseq}{
    The wind speed bins.
  }
  \item{palette}{
    Colour palette to be used.
  }
  \item{countmax}{
    Maximum for ylim.
  }
  \item{labels}{
    Labels for wind speed.
  }
  \item{title}{
    The title of the plot.
  }
  \item{xlab}{
    The x-axis label.
  }
  \item{ylab}{
    The y-axis label.
  }
  \item{fill.name}{
    The name of the variable filling the rose petal. Default is
    "Wind speed (m/s)". 
  }
  \item{debug}{
    An integer. If greater than zero then the function will run in debug mode.
  }
}
\examples{
## Close any previously open graphic devices
graphics.off()

## Load the data
data(fluxes)

## Plot the wind rose
plotWindrose(data=fluxes,spd="wind_speed",dir="wind_dir")
}

