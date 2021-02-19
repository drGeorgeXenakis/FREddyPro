lue <-
function (temp, par, vpd, asw, Topt, Tmin, Tmax, StandAge, fullCanAge, 
    maxASW, SWconst, SWpower, fN0, fNn, FR, k, lai, kappa, alphaCx, 
    A) 
{
    if (fullCanAge > 0 & StandAge < fullCanAge) {
        CanCover = (StandAge + 0.01)/fullCanAge
    }
    else {
        CanCover = 1
    }
    lightIntcptn = (1 - (exp(-k * lai/CanCover)))
    apar = par * lightIntcptn * CanCover
    if (temp <= Tmin || temp >= Tmax) {
        fT = 0
    }
    else {
        fT = ((temp - Tmin)/(Topt - Tmin)) * ((Tmax - temp)/(Tmax - 
            Topt))^((Tmax - Topt)/(Topt - Tmin))
    }
    fVPD = exp(-kappa * vpd)
    moistRatio = asw/maxASW
    fSW = 1/(1 + ((1 - moistRatio)/SWconst)^SWpower)
    if (fNn == 0) {
        fNutr = 1
    }
    else {
        fNutr = 1 - (1 - fN0) * (1 - FR)^fNn
    }
    PhysMod = pmin(fVPD, fSW)
    alphaC = alphaCx * fT * PhysMod * fNutr
    GPP = alphaC * apar
    tempK <- temp + 273.15
    E0 = 308.56
    T0 = 227.13
    Re = A * exp(-E0/(tempK - T0))
    NEE = Re - GPP
    returnList = list(GPP = GPP, Re = Re, NEE = NEE)
    return(returnList)
}
