\name{percentQC}
\alias{percentQC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{percentQC
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
percentQC(data, var = "co2_flux", qc_var = "qc_co2_flux", write = FALSE, dir = "./")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{qc_var}{
%%     ~~Describe \code{qc_var} here~~
}
  \item{write}{
%%     ~~Describe \code{write} here~~
}
  \item{dir}{
%%     ~~Describe \code{dir} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, var = "co2_flux", qc_var = "qc_co2_flux", write = FALSE, 
    dir = "./") 
{
    qc = paste(paste(deparse(substitute(data)), "$", qc_var, 
        sep = ""))
    v = paste(paste(deparse(substitute(data)), "$", var, sep = ""))
    qcflag0 <- which(eval(parse(text = qc)) == 0)
    qcflag1 <- which(eval(parse(text = qc)) == 1)
    qcflag2 <- which(eval(parse(text = qc)) == 2)
    gap. <- which(is.na(eval(parse(text = v))))
    prc0.all = round((length(qcflag0)/(length(eval(parse(text = v))))) * 
        100, 2)
    prc1.all = round((length(qcflag1)/(length(eval(parse(text = v))))) * 
        100, 2)
    prc2.all = round((length(qcflag2)/(length(eval(parse(text = v))))) * 
        100, 2)
    prcGap <- round((length(gap.)/length(eval(parse(text = v)))) * 
        100, 2)
    if (write) {
        path = paste(dir, "/percent_qc_flags.txt", sep = "")
        sink(path)
        print(paste(prc0.all, "\%", "of QC 0 for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        print(paste(prc1.all, "\%", "of QC 1 for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        print(paste(prc2.all, "\%", "of QC 2 for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        print(paste(prcGap, "\%", "of gap for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        sink()
    }
    else {
        print(paste(prc0.all, "\%", "of QC 0 for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        print(paste(prc1.all, "\%", "of QC 1 for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        print(paste(prc2.all, "\%", "of QC 2 for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        print(paste(prcGap, "\%", "of gap for", var, "in the dataset", 
            sep = " "), quote = FALSE)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
