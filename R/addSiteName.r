addSiteName <-
function (data, name) 
{
    for (i in 1:nrow(data)) {
        data$site[i] = name
    }
    return(data)
}
.__global__ <-
"."
