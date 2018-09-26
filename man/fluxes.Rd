\name{fluxes}
\alias{fluxes}
\docType{data}
\title{
  Fluxes from Harwood forest
}
\description{
  Standard EddyPro full output for Harwood forest, Northumberland, Great
  Britain. The eddy covariance tower is managed by Forest Research
}
\usage{data(fluxes)}
%% \source{
%% %%  ~~ reference to a publication or URL from which the data were obtained ~~
%% }
\examples{
## Load the data
data(fluxes)

## Explore the variables in the data frame
str(fluxes)
## Quickly view some variables
plot(fluxes[,c(8,10,12,14)])
}
