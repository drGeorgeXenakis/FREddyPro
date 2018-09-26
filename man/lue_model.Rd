\name{lue_model}
\alias{lue_model}
\title{
  Light use efficiency model
}
\description{
  A simple light use efficiency model for fitting NEE data. 
}
\usage{
lue_model(temp, par, vpd, swc, Topt, Tmin, Tmax, StandAge, fullCanAge,
k, lai, gamma, kappa, thetaWP, thetaFC, alpha, v, A, epsilon)
}
\arguments{
  \item{temp}{
    Air temperature (K)
  }
  \item{par}{
    Photosynthetic active radiation (umol/m2/s)
  }
  \item{vpd}{
    Vapour pressure deficit (Pa)
  }
  \item{swc}{
    Soil water content (unitless)
  }
  \item{Topt}{
    Optimal temperature for growth (oC)
  }
  \item{Tmin}{
    Minimum temperature for growth (oC)
  }
  \item{Tmax}{
    Maximum temperature for growth (oC)
  }
  \item{StandAge}{
    Age of the stand (years)
  }
  \item{fullCanAge}{
    Age at full canopy coluser (years)
  }
  \item{k}{
    Light extinction coefficient (unitless)
  }
  \item{lai}{
    Leaf area index (unitless)
  }
  \item{gamma}{
    Empirical parameter for light modifiers
  }
  \item{kappa}{
    Empirical parameter for vapour pressure modifiers
  }
  \item{thetaWP}{
    Volumetric water content at wilting point
  }
  \item{thetaFC}{
    Volumetric water content at field capacity
  }
  \item{alpha}{
    Empirical parameter for soil water
  }
  \item{v}{
    Empirical parameter for soil water 
  }
  \item{A}{
    Empirical parameters for ecosystem respiration
  }
  \item{epsilon}{
    Empirical potential light use efficiency
  }
}
\details{
  The model is based on the simple LUE model by Makela et al. 2008 with
  some small modification including functions for light interception,
  temperature modifier, and ecosystem respiration. Light interception
  and temperature modifiers are thosed used in the popular
  ecophysiological model 3-PG by Landsberg and Sands 1997. Light
  interception is based on Beer's law using a light extinction
  coefficient, leaf area index and canopy cover. Canopy cover is
  calculated from current stand age, and the age at full canopy
  cover. Ecosystem respiration is estimated using Arrhenius type
  function (Lloyd and Taylor, 1994) dependent on temperature.
}
\value{
  The output of the function is a list with three elements including
  gross primary production (GPP), ecosystem respiration (Re) and net
  ecosystem exchange (NEE).
}
\references{
  Landsberg, J.J. and Waring, R.H. (1997). A generalised model of forest
  productivity using simplified concepts of radiationuse efficiency,
  carbon balance and partitioning. Forest Ecology and Management. 95,
  209-228
  
  Lloyd, J. and Taylor, A. (1994). On the temperature dependence of soil
  respiration. Function Ecology. 8, 315-323.
  
  Makela, A., Pulkkinen, M., Kolari, P., Lagergren, F., Berbigier, P.,
  Lindroth, A., Loustau, D., Nikinmaa, E., Vesala, T. and Hari,
  P. (2008). Developing an empirical model of stand GPP with the LUE
  approach: analysis of eddy covariance data at five ontrasting conifer
  sites in Europe. Global Change Biology. 14, 92-108.
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

## Input
PPFD=2500
swc=0.35

## Parameters
k = 0.5
Topt = 12
Tmin = -2
Tmax = 35
lai = 6
StandAge = 41
fullCanAge = 15

gamma = 0.0003
kappa = -0.0006
alpha = 0.3
v = 0.5
thetaWP = 0.4
thetaFC = 0.7
A = 200
epsilon = 0.0164 ## umol C / umol APAR

model<-lue_model( fluxes$air_temperature, 2500, fluxes$VPD, swc=0.35,
Topt, Tmin, Tmax, StandAge, fullCanAge, k, lai, gamma, kappa, thetaWP,
thetaFC, alpha, v, A, epsilon )

par(mfrow=c(3,1))
plot(model$NEE,type='l')
plot(model$GPP,type='l')
plot(model$Re,type='l')

}
