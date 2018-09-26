\name{cleanFluxes}
\alias{cleanFluxes}
\title{
  Cleaning and de-spiking fluxes
}
\description{
  The main function for cleaning and de-spiking post-processed
  fluxes. There are five ways to clean/de-spike gas fluxes based on 1)
  QC flags, 2) standard deviation of negative and positive fluxes, 2)
  flux distribution for each hour of the day, 4) mean AGC value of the
  IRGA and 5) u* filtering. There are also three ways to clean heat and
  water fluxes based on 1) QC flags, 2) a threshold value and on 3)
  standard deviation of negative and positive fluxes.
}
\usage{
cleanFluxes(data, gas = "co2_flux", qcFlag = 2, sdCor = FALSE, sdTimes =
1, distCor = FALSE, agcCor = FALSE, agcVal = NULL, ustar = NULL, plot
= FALSE, write = FALSE, outputFile, thresholdList = list(H = NULL, LE =
NULL, Tau = NULL, h2o = NULL), timesList = list(H = NULL, LE = NULL, Tau
= NULL, h2o = NULL), sunset = 19, sunrise = 6, na.value = "NaN")
}
\arguments{
  \item{data}{
    The name of the data frame to clean and de-spike.
}
  \item{gas}{
    A character input giving the name of the gas to clean. The default
    value is for CO2.
}
  \item{qcFlag}{
    The QC flag to clean. Default value is 2 using the Mauder and Foken
    (2004) flagging system. qcFlag=NULL will bypass the flag removal
    system and no data will be removed. Function can be used with other
    flagging systems (e.g., 1-9 by Foken 2003) but an array must be
    given of values to remove e.g., qcFlag=c(7,8,9).
}
  \item{sdCor}{
    Logical. If TRUE de-spiking based on standard deviation
    is applied.
}
  \item{sdTimes}{
    A number showing how many times the gas flux data have to be greater
    than the SD to remove them as spikes. The default value is 1 SD.
}
  \item{distCor}{
    Logical. If TRUE gas flux data are de-spiked using a
    distribution calculated for every half hour.
}
  \item{agcCor}{
    Logical. If TRUE data with a mean value between 50-60\% are removed.
}
  \item{agcVal}{
    A character input giving the name of the mean AGC in the data.
}
  \item{ustar}{
    This input can be either logical or numeric. If logical and TRUE
    then ustar filtering is applied using the Papale et al. 2006
    method. Sunset and sunrise times should also be provided for more
    accurate definition of the night time data.
}
  \item{plot}{
    Logical. If TRUE two multiplot outputs are produced showing
    all major variables before and after cleaning and de-spiking.
}
  \item{write}{
    Logical. If TRUE the new clean data frame will be written
    into a new csv file. The output file name should also be given.
}
  \item{outputFile}{
    A character input giving the name of the output file to write the
    clean data. String can include full path name.
}
  \item{thresholdList}{
    A list giving the threshold for which if greater and lower data will
    be removed. The list can include sensible heat (H), latent heat
    (LE), momentum flux (Tau), and water flux (h2o).
}
  \item{timesList}{
    A list giving how many times a flux should be greater than the SD to
    be a spike. The list can include sensible heat (H), latent heat
    (LE), momentum flux (Tau), and water flux (h2o).
}
  \item{sunset}{
    The time of sunset as a real number (0-23) 
}
  \item{sunrise}{
    The time of sunrise as a real number (0-23) 
}
  \item{na.value}{
    A string or a number showing NA values in the data set
}
}
\details{
  By default the function removes QC flag 2, assuming data were flagged
  for quality using Mauden and Foken (2004) system.

  To clean data using standard deviation, the mean and standard
  deviation of negative and positive values is calculated
  separately. Data greater than a user-defined times of the standard
  deviation are removed

  For distributional cleaning, the half-hourly destribution using the
  complete dataset and the 5th and 95th quantiles are calculated. Values
  outside these quantiles are removed.

  Mean AGC is calculated by EddyPro if a diagnostics value is given
  during processing of raw data. AGC usually should be between 50 -
  60\%. Values outside the threshold will be removed. Use with caution
  as it may remove a large number of data

  U* filtering is standard procedure. For the FREddyPro function we used
  the procedure described by Papale et al. 2006 . Air temperature is
  classes and within each temperature class u* is grouped in 20 classes.
  For more details see Papale et al. 2006. 
}
\references{
  Papale D, Reichstein M, Aubinet M, Canfora E, Bernhofer C, Kutsch W,
  Longdoz B, Rambal S, Valentini R, Vesala T & et al. (2006)
  Towards a standardized processing of Net Ecosystem Exchange measured
  with eddy covariance technique: algorithms and uncertainty estimation. 
  Biogeosciences, Copernicus GmbH, 3, 571 - 583 
}
\author{
  Georgios Xenakis
}
\examples{
## Load the data
data(fluxes)

## Clean data using 3 times the SD for both gas and heat fluxes.
## Also use some thresholds for head fluxes.
fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,distCor=TRUE,timesList=3,
thresholdList=list(H=c(-100,1000),LE=c(-100,1000)),plot=TRUE)

}
