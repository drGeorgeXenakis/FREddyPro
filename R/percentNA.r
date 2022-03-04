percentNA <-
function (var) 
{
    if (any(is.na(var))) {
        na = which(is.na(var))
        pc <- round((length(na)/length(var)) * 100, 2)
        return(pc)
    }
    else {
        print("No NAs found. You lucky person!", quote = FALSE)
    }
}
