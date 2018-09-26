calculateFootprint <-
function (df, displacement, stability = NULL, fetch = 500, grid = 200, 
    height, lowerDay = NULL, upperDay = NULL) 
{
    df <- df[which(!is.na(df$L)), ]
    if (is.null(lowerDay) & !is.null(upperDay)) {
        df <- df[which(df$DOY <= upperDay), ]
    }
    else if (!is.null(lowerDay) & is.null(upperDay)) {
        print("In not null")
        df <- df[which(df$DOY >= lowerDay), ]
    }
    else if (!is.null(lowerDay) & !is.null(upperDay)) {
        df <- df[which(df$DOY >= lowerDay & df$DOY <= upperDay), 
            ]
    }
    df$z = (df$X.z.d..L * df$L) + displacement
    df$zol = df$z/df$L
    if (is.null(stability)) {
        stab = seq(1, nrow(df), 1)
    }
    else if (stability == 1) {
        stab = which(df$zol < -0.0625)
    }
    else if (stability == 2) {
        stab = which(df$zol >= -0.0625 & df$zol <= 0.0625)
    }
    else if (stability == 3) {
        stab = which(df$zol > 0.0625)
    }
    df <- df[stab, ]
    footprint = Average(fetch = fetch, height = height, grid = grid, 
        df$wind_speed, df$wind_dir, df$u., df$zol, sqrt(df$v_var))
    return(footprint)
}
