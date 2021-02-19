.__global__ <-
"."
qcClean <-
function (var, qcVar, qc) 
{
    if (length(qc) != 1) {
        for (i in 1:length(qc)) {
            qc.index <- which(qcVar == qc[i])
            var[qc.index] <- NA
        }
    }
    else {
        qc.index <- which(qcVar == qc)
        var[qc.index] <- NA
    }
    return(var)
}
