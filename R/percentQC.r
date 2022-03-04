percentQC <-
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
        print(paste(prc0.all, "%", "of QC 0 for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        print(paste(prc1.all, "%", "of QC 1 for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        print(paste(prc2.all, "%", "of QC 2 for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        print(paste(prcGap, "%", "of gap for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        sink()
    }
    else {
        print(paste(prc0.all, "%", "of QC 0 for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        print(paste(prc1.all, "%", "of QC 1 for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        print(paste(prc2.all, "%", "of QC 2 for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        print(paste(prcGap, "%", "of gap for", var, "in the dataset", 
            sep = " "), quote = FALSE)
    }
}
