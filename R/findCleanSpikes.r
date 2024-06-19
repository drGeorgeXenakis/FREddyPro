findCleanSpikes <-
function (data, x, qc = NULL, qcFlag = NULL, times = 3, quantiles = c(0.050000000000000003, 
    0.94999999999999996), group = hour, plotSpikes = FALSE, removeSpikes = FALSE, 
    returnDataFrameOnly = FALSE, limits = c(-50, 30), breaks = seq(-50, 
        30, 20)) 
{
    data <- data %>% select(-one_of("spikes", "fn1", "fn2"))
    outList <- list()
    v <- enquo(x)
    qc_v <- enquo(qc)
    grp <- enquo(group)
    if (!missing(qc)) 
        data <- data %>% mutate(`:=`(!!qc_v, if_else(is.na(!!v), 
            NA_real_, !!qc_v)))
    if (missing(qc)) {
        dataSpk <- data %>% mutate(spikes = 0)
    }
    else {
        if (is.null(qcFlag)) {
            dataSpk <- data
        }
        else {
            dataSpk <- data %>% mutate(`:=`(!!v, if_else(!(!!qc_v %in% 
                qcFlag), !!v, NA_real_)), `:=`(!!qc_v, if_else(!(!!qc_v %in% 
                qcFlag), !!qc_v, NA_real_)), spikes = if_else(!(!!qc_v %in% 
                qcFlag), 0, -1))
        }
    }
    mean.positive <- dataSpk %>% filter(!!v >= 0) %>% select(!!v) %>% 
        summarise(mean = mean(!!v, na.rm = T)) %>% as.numeric()
    mean.negative <- dataSpk %>% filter(!!v < 0) %>% select(!!v) %>% 
        summarise(mean = mean(!!v, na.rm = T)) %>% as.numeric()
    sd.positive <- dataSpk %>% filter(!!v >= 0) %>% select(!!v) %>% 
        summarise(sd = sd(!!v, na.rm = T)) %>% as.numeric()
    sd.negative <- dataSpk %>% filter(!!v < 0) %>% select(!!v) %>% 
        summarise(sd = sd(!!v, na.rm = T)) %>% as.numeric()
    if (dataSpk %>% filter(!!v >= 0) %>% summarise(n()) != 0) 
        dataSpk <- dataSpk %>% mutate(spikes = if_else(!!v > 
            (mean.positive + times * sd.positive), spikes + 1, 
            spikes + 0))
    if (dataSpk %>% filter(!!v < 0) %>% summarise(n()) != 0) 
        dataSpk <- dataSpk %>% mutate(spikes = if_else(!!v < 
            (mean.negative - times * sd.negative), spikes + 1, 
            spikes + 0))
    p_funs <- map(quantiles, ~partial(quantile, probs = .x, na.rm = TRUE))
    quantDF <- dataSpk %>% filter(spikes == 0) %>% group_by(!!grp) %>% 
        summarise_at(vars(!!v), .funs = c(p_funs))
    dataSpk <- dataSpk %>% left_join(quantDF, by = quo_name(grp)) %>% 
        mutate(spikes = if_else(!!v > fn2, spikes + 1, spikes + 
            0), spikes = if_else(!!v < fn1, spikes + 1, spikes + 
            0))
    if (plotSpikes) {
        if (missing(qc)) {
            pDiurnal <- ggplot(data = dataSpk %>% drop_na(spikes)) + 
                geom_point(aes(x = !!grp, y = !!v, colour = as.factor(spikes)), 
                  shape = 16) + geom_line(aes(x = !!grp, y = fn1)) + 
                geom_line(aes(x = !!grp, y = fn2), colour = "blue") + 
                theme_bw() + scale_y_continuous(limits = limits, 
                breaks = breaks) + theme(panel.grid.major = element_line(colour = "grey70"), 
                panel.grid.minor = element_line(colour = "grey70"))
            pTimeseries <- ggplot(data = dataSpk %>% drop_na(spikes)) + 
                geom_point(aes(x = timestamp, y = !!v, colour = as.factor(spikes)), 
                  shape = 16) + theme_bw() + scale_y_continuous(limits = limits, 
                breaks = breaks) + theme(panel.grid.major = element_line(colour = "grey70"), 
                panel.grid.minor = element_line(colour = "grey70"))
            outList[[2]] <- pDiurnal
            outList[[3]] <- pTimeseries
        }
        else {
            pDiurnal <- ggplot(data = dataSpk %>% drop_na(spikes)) + 
                geom_point(aes(x = !!grp, y = !!v, colour = as.factor(spikes), 
                  shape = as.factor(!!qc_v))) + geom_line(aes(x = !!grp, 
                y = fn1)) + geom_line(aes(x = !!grp, y = fn2), 
                colour = "blue") + theme_bw() + scale_y_continuous(limits = limits, 
                breaks = breaks) + theme(panel.grid.major = element_line(colour = "grey70"), 
                panel.grid.minor = element_line(colour = "grey70"))
            pTimeseries <- ggplot(data = dataSpk %>% drop_na(spikes)) + 
                geom_point(aes(x = timestamp, y = !!v, colour = as.factor(spikes), 
                  shape = as.factor(!!qc_v))) + theme_bw() + 
                scale_y_continuous(limits = limits, breaks = breaks) + 
                theme(panel.grid.major = element_line(colour = "grey70"), 
                  panel.grid.minor = element_line(colour = "grey70"))
            outList[[2]] <- pDiurnal
            outList[[3]] <- pTimeseries
        }
    }
    if (removeSpikes) {
        dataDspk <- dataSpk %>% mutate(`:=`(!!v, if_else(spikes == 
            0, !!v, NA_real_)))
        if (!missing(qc)) 
            dataDspk <- dataDspk %>% mutate(`:=`(!!qc_v, if_else(spikes == 
                0, !!qc_v, NA_real_)))
        outList[[1]] <- dataDspk
    }
    else {
        outList[[1]] <- dataSpk
    }
    if (returnDataFrameOnly) {
        return(as_tibble(outList[[1]]))
    }
    else {
        return(outList)
    }
}
