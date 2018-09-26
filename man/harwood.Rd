\name{harwood}
\alias{harwood}
\docType{data}
\title{
  CSV EddyPro output file for Harwood forest
}
\description{
  Standard EddyPro full output for Harwood forest, Northumberland, Great
  Britain. The eddy covariance tower is managed by Forest Research
}
\usage{data(harwood)}
%% \source{
%% %%  ~~ reference to a publication or URL from which the data were obtained ~~
%% }
\examples{
## Load the data
data(harwood)

## Write the data as csv
write.table(harwood,file="harwood.csv",sep=",",quote=FALSE,row.names=FALSE)
}
