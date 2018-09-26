lue_model <-
function (temp, par, vpd, swc, Topt, Tmin, Tmax, StandAge, fullCanAge, 
    k, lai, gamma, kappa, thetaWP, thetaFC, alpha, v, A, epsilon) 
{
    if (fullCanAge > 0 & StandAge < fullCanAge) {
        CanCover = (StandAge + 0.01)/fullCanAge
    }
    else {
        CanCover = 1
    }
    lightIntcptn = (1 - (exp(-k * lai/CanCover)))
    apar = par * lightIntcptn * CanCover
    fL = 1/(gamma * apar + 1)
    Tc = temp - 273.15
    if (Tc <= Tmin || Tc >= Tmax) {
        fT = 0
    }
    else {
        fT = ((Tc - Tmin)/(Topt - Tmin)) * ((Tmax - Tc)/(Tmax - 
            Topt))^((Tmax - Topt)/(Topt - Tmin))
    }
    fD = exp(kappa * vpd)
    Wk = pmin((swc - thetaWP)/(thetaFC - thetaWP), 1)
    fW = (1 + ((1 - Wk)/(alpha))^v)^-1
    GPP = epsilon * apar * fL * fD * fW * fT
    E0 = 308.56
    T0 = 227.13
    Re = A * exp(-E0/(temp - T0))
    NEE = Re - GPP
    returnList = list(GPP = GPP, Re = Re, NEE = NEE)
    return(returnList)
}
