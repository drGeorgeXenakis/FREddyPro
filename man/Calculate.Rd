\name{Calculate}
\alias{Calculate}
\title{
  Kormann and Meixner 2001 flux footprint model
}
\description{
  Performs the footprint calculation for a single set of inputs
}
\usage{
Calculate(fetch = 500, height = 3, grid = 200, speed, direction, uStar,
zol, sigmaV)
}

\arguments{
  \item{fetch}{
    The upwind distance over which you want the calculation domain to extend
    Note that the footprint will be normalized to that domain so footprints that 
    extend well beyond the domain will be overrepresented in
}
  \item{height}{
    The z-d height of the flux system
}
  \item{grid}{
    The total calculation grid size. The default is 200 which would mean
    the footprint would be represented in a 200x200 array corresponding
    to dimensions of two times the fetch on each side of the grid.
}
  \item{speed}{
    Single values of wind speed at the flux system
}
  \item{direction}{
    Single values of the direction wind is coming from at the flux system
}
  \item{uStar}{
    Single values of friction velocity
}
  \item{zol}{
    Single values of Monin Ohbukov stability parameter 
}
  \item{sigmaV}{
    Single values of the standard deviation of the cross-stream wind component
}
}
\value{
  Output is a list object with a 3 elements including:
  
  footprint:  the array holding the footprint probabilities 
  FPn:        the array holding the grid point distance north of the flux system
  FPe         the array holding the grid point distance east of the flux system              
}
\references{
  Kormann, R., and Meixner, F.X. (2001). An analytical footprint model
  for non-neutral stratification, Boundary-Layer Meteorology. 99, 207-224.
}
\author{
  Original author: Robert Clement, University of Edinburgh. R
  adaptation: Georgios Xenakis, Forest Research
}
\note{
  This code impliments the footprint model of   Kormann and  Meixner
  'An Analytical Footprint Model For Non-Neutral Stratification' ,
  Boundary-Layer Meteorology, 2001, v99, Issue 2, pp 207-224
  
  Modifications have been added to determine the footprint for a two-dimensional area
  surrounding the flux tower.  Calculations can be done for a single flux footprint, or
  an average of weighted or non-weighted footprints.
  
  This code was written by Robert Clement, (robert.clement@ed.ac.uk) 
  
  Copyright (c) [2015] [Robert Clement]
  
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
  
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
  
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.  
}
\examples{
## Load the data
data(fluxes)

## Clean fluxes
fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,timesList=3,distCor=TRUE,
                   thresholdList=list(H=c(-100,1000),LE=c(-100,1000)))	

## Use data without NA values
fluxes_noNA<-fluxes[which(!is.na(fluxes$L)),]

## Displacement height
d=17.42

## Calculate z
fluxes_noNA$z=(fluxes_noNA$X.z.d..L*fluxes_noNA$L)+d

## Calculate zol
fluxes_noNA$zol=fluxes_noNA$z/fluxes_noNA$L

## Calculate the footprint
footprint=Calculate(fetch=500,height=33,grid=200,fluxes_noNA$wind_speed[853],
fluxes_noNA$wind_dir[853],fluxes_noNA$u.[853],fluxes_noNA$zol[853],
sqrt(fluxes_noNA$v_var[853]))

## You can also use the plot.footprint function of FREddyPro to plot the result
plotFootprint(footprint)
}
