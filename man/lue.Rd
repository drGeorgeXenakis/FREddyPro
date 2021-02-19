\name{lue}
\alias{lue}
\title{Light use efficiency model}
\description{
  A simple light use efficiency model for fitting NEE data. 
}
\usage{
lue(temp,par,vpd,asw,Topt,Tmin,Tmax,
StandAge,fullCanAge,maxASW,SWconst,SWpower,fN0,fNn,FR,
k,lai,kappa,alphaCx,A)
}

\arguments{
  \item{temp}{
    Air temperature (oC)
  }
  \item{par}{
    Photosynthetic active radiation (umol m-2 s-1)
  }
  \item{vpd}{
    Vapour pressure deficit (Pa)
  }
  \item{asw}{
    Available soil water (mm)
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
    Age at full canopy closure (years)
  }
  \item{maxASW}{
    Maximum available soil water (mm)
  }
  \item{SWconst}{
    Moisture ratio deficit for soil water modifier equal to 0.5 (unitless)
  }
  \item{SWpower}{
    Power of moisture ratio deficit (unitless)
  }
  \item{fN0}{
    Value of nutrient modifier when fertility ratining equals to zero
  }
  \item{fNn}{
    Power of (1-FR) in the nutrient modifier
  }
  \item{FR}{
    Fertility rating from 0 (infertile) to 1 (very fertile) soil (unitless)
  } 
  \item{k}{
    Light extinction coefficient (unitless)
  }
  \item{lai}{
    Leaf area index (unitless)
  }
  \item{kappa}{
    Empirical parameter for vapour pressure modifiers
  }
  \item{alphaCx}{
    Maximum quantum use efficiency (umol CO2 / umolPAR)
  }
  \item{A}{
    Empirical parameters for ecosystem respiration
  }
}
\details{
  This is a simplification of the 3PG model by Landsberg et al. 1997
  with a couple of modifications. There is no age modifier, assuming
  that age related decline has not have impact on short-term (half hour
  over a few years) net ecosystem exchage. Maximum quantum use
  efficiency is reduced by a series of modifiers from 0 to 1, including
  temperature, soil water, nutrients, vapour pressure deficit and soil
  water. Soil nutrient of the site is represented with the fertility
  rating parametes range from 0 (infertile) to 1 (very fertile). Light
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

}

\author{
  Georgios Xenakis
}

%% \examples{
%% ## Close any previously open graphic devices
%% graphics.off()

%% ## Load the data
%% data(fluxes)

%% ## Clean the fluxes
%% fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,timesList=3,distCor=TRUE,
%% thresholdList=list(H=c(-100,1000),LE=c(-100,1000)))

%% ## Input
%% PPFD=2500
%% swc=0.35

%% ## Parameters
%% k = 0.5
%% Topt = 12
%% Tmin = -2
%% Tmax = 35
%% lai = 6
%% StandAge = 41
%% fullCanAge = 15

%% gamma = 0.0003
%% kappa = -0.0006
%% alpha = 0.3
%% v = 0.5
%% thetaWP = 0.4
%% thetaFC = 0.7
%% A = 200
%% epsilon = 0.0164 ## umol C / umol APAR

%% model<-lue_model( fluxes$air_temperature, 2500, fluxes$VPD, swc=0.35,
%% Topt, Tmin, Tmax, StandAge, fullCanAge, k, lai, gamma, kappa, thetaWP,
%% thetaFC, alpha, v, A, epsilon )

%% par(mfrow=c(3,1))
%% plot(model$NEE,type='l')
%% plot(model$GPP,type='l')
%% plot(model$Re,type='l')

%% }
