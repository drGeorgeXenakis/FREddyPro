Average <-
function (fetch = 500, height = 3, grid = 200, speed, direction, 
    uStar, zol, sigmaV, weights = NULL) 
{
    grid = 2 * floor(grid/2)
    lens = c(length(speed), length(direction), length(uStar), 
        length(zol), length(sigmaV))
    if (!is.null(weights)) {
        lens = c(lens, length(weights))
    }
    if (min(lens) == max(lens)) {
        ids = !is.na(speed)
        idd = !is.na(direction)
        idu = !is.na(uStar)
        idz = !is.na(zol)
        idv = !is.na(sigmaV)
    }
    idok = ids & idd & idu & idz & idv
    if (!is.null(weights)) {
        idw = !is.na(weights)
        idok = idok & idw
    }
    speed = speed[idok]
    direction = direction[idok]
    uStar = uStar[idok]
    zol = zol[idok]
    sigmaV = sigmaV[idok]
    if (!is.null(weights)) {
        weights = weights[idok]
    }
    if (length(speed) >= 1) {
        avg_footprint = list(Probability = matrix(0, nrow = grid, 
            ncol = grid))
        if (!is.null(weights)) {
            avg_footprint = list(Probability = matrix(0, nrow = grid, 
                ncol = grid), WeightedProbability = matrix(0, 
                nrow = grid, ncol = grid))
        }
        finput = t(mapply(list, speed, direction, uStar, zol, 
            sigmaV))
        icnt = 0
        for (i in 1:nrow(finput)) {
            ftp <- Calculate(fetch, height, grid, finput[[i, 
                1]], finput[[i, 2]], finput[[i, 3]], finput[[i, 
                4]], finput[[i, 5]])
            if (any(!is.na(ftp$footprint))) {
                avg_footprint$Probability = avg_footprint$Probability + 
                  ftp$footprint
                if (!is.null(weights)) {
                  avg_footprint$WeightedProbability = avg_footprint$WeightedProbability + 
                    ftp$footprint * weights[i]
                }
            }
            icnt = icnt + 1
        }
    }
    avg_footprint$Probability = avg_footprint$Probability/icnt
    returnList = list(Probability = avg_footprint$Probability, 
        FPe = ftp$FPe, FPn = ftp$FPn, WeightedProbability = avg_footprint$WeightedProbability)
    return(returnList)
}
Calculate <-
function (fetch = 500, height = 3, grid = 200, speed, direction, 
    uStar, zol, sigmaV) 
{
    grid = 2 * floor(grid/2)
    sigmaY = matrix(0, nrow = grid, ncol = grid)
    twopisigma = matrix(0, nrow = grid, ncol = grid)
    DispY = matrix(0, nrow = grid, ncol = grid)
    Pfp = matrix(0, nrow = grid, ncol = grid)
    Pf = matrix(0, nrow = grid, ncol = grid)
    auPlume = matrix(1, nrow = grid, ncol = grid)
    linn = seq(99.5, -99.5, length = grid)
    line = seq(-99.5, 99.5, length = grid)
    linn = linn * 2 * fetch/199
    line = line * 2 * fetch/199
    m = length(line)
    n = length(linn)
    FPe = matrix(rep(line, each = n), nrow = n)
    FPn = matrix(rep(linn, m), nrow = n)
    FPd = sqrt(FPn^2 + FPe^2)
    FPa = atan2(FPe, FPn) * 180/pi
    FPa = FPa - direction
    FPx = cos(FPa/57) * FPd
    FPy = sin(FPa/57) * FPd
    zt = ifelse(zol > 0, 0, (1 - 16 * zol)^0.25)
    phim = ifelse(zol > 0, 1 + 5 * zol, (1 - 16 * zol)^-0.25)
    phic = ifelse(zol > 0, 1 + 5 * zol, (1 - 16 * zol)^-0.5)
    psim = ifelse(zol > 0, 5 * zol, -2 * log((1 + zt)/2) - log((1 + 
        zt * zt)/2) + 2 * atan(zt) - pi/2)
    nn = ifelse(zol > 0, 1/phic, (1 - 24 * zol)/(1 - 16 * zol))
    eddydif = 0.41 * uStar * height/phic
    mm = uStar * phim/(0.41 * speed)
    rr = 2 + mm - nn
    mu = (1 + mm)/rr
    alpu = speed/(height^mm)
    alpk = eddydif/(height^nn)
    xi = alpu * height^rr/(((rr)^2) * alpk)
    fgamma = gamma(mu)
    idup = FPx > 0
    Pf[idup] = (1/fgamma) * (xi^mu)/(FPx[idup]^(1 + mu)) * exp(-1 * 
        xi/FPx[idup])
    uPlume = (gamma(mu)/gamma(1/rr))
    uPlume = uPlume * ((rr * rr * 0.4/speed)^(mm/rr))
    auPlume[idup] = uPlume * (speed * (FPx[idup]^(mm/rr)))
    sigmaY[idup] = sigmaV * FPx[idup]/auPlume[idup]
    twopisigma[idup] = 1/((sqrt(2 * pi) * sigmaY[idup]))
    DispY[idup] = twopisigma[idup] * exp(-1 * FPy[idup] * FPy[idup]/(2 * 
        sigmaY[idup] * sigmaY[idup]))
    Pfp[idup] = Pf[idup] * DispY[idup]
    footprint = Pfp/sum(sum(Pfp))
    returnList = list(footprint = footprint, FPe = FPe, FPn = FPn)
    return(returnList)
}
