1. Make the plots of the heatmaps (dt vs width)
  - The matrices for: gr, dmax, gmax, b, etc...

2. Create the "plot parameters"
  - gamma vs. par (seq(min(PAR) to max(PAR), by=100))
  - delta vs. size classes (V)
    - seq size classes (V): seq(min(V), max(V), by 100))

3. Plot the cell distribtions for each input
    - for this version of popcylce... we ran into the issue of having too much noise
    when we increase the resolution of the size distribution by increasing the number
    of size classes (by decreasing the width of each size class)
    - the noise can mess up the resnorm





library(ssPopModel)
library(gplots)

#John
path.to.data <- "~/ssPopModel_sensitivity_test/ssPopModel_sensitivity_test_data"
path.to.git.repository <- "~/ssPopModel_sensitivity_test"
setwd(path.to.git.repository)

list.dist  <- list.files("input", "size.distribution_Prochlorococcus",full.names=T)
load(list.dist[1])
plot.size.distribution(distribution[[1]][37:61], type='l', lwd=2)


## MERGE MODEL OUTPUT (dt)
list.output <- list.files("output", "size",full.names=T)
DF <- NULL

for(path.distribution in list.output){
    #path.distribution <- list.output[1]
    print(path.distribution)
    load(path.distribution)
    file.name <- unlist(list(strsplit(basename(path.distribution), "_")))
    origin <- file.name[1]
    size <- as.numeric(file.name[3])
    dt <- as.numeric(file.name[4])

    # if(origin == "biomass"){
    #     params <- model2[,2][[1]]
    #     gr <- model2[,2][[2]]
    # }

    if(origin == "size"){
        params <- model1[,2][[1]]
        gr <- model1[,2][[2]]
    }
    df <- data.frame(origin, size, dt, params,gr=sum(gr,na.rm=T))
    DF <- data.frame(rbind(DF, df))
}


path.distribution <- list.output[100]
load(path.distribution)
summary(path.distribution)



function (merged.estimates)
{
    jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF",
        "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    volbins <- unique(as.numeric(row.names(merged.estimates$Vproj)))
    para <- merged.estimates$estimates
    h2.time <- para$h.time
    cat <- length(volbins)
    del <- matrix(nrow = length(h2.time), ncol = cat)
    for (i in 1:cat) del[, i] <- para$h2.dmax.mean * (volbins[i]/max(volbins))^para$h2.b.mean/(1 +
        (volbins[i]/max(volbins))^para$h2.b.mean)
    par(mfrow = c(2, 1), mar = c(4, 4, 4, 4), las = 1)
    plot(volbins, del[1, ], ylim = c(0, max(del, na.rm = T)),
        type = "l", col = "#00007F", lwd = 2, xlab = "Cell volume",
        ylab = paste("Delta (per", 10, "min)"))
    for (i in 2:nrow(del)) points(volbins, del[i, ], type = "l",
        col = jet.colors(nrow(del))[cut(as.numeric(h2.time),
            nrow(del))][i], lwd = 2)
    ylim <- par("usr")[c(3, 4)]
    xlim <- par("usr")[c(1, 2)]
    color.legend(xlim[2] - diff(xlim)/40, ylim[1], xlim[2], ylim[2],
        legend = format(as.POSIXct(range(h2.time, na.rm = T),
            origin = "1970-01-01"), "%d %b"), rect.col = jet.colors(100),
        gradient = "y", align = "rb")
    max <- max(para$h2.gmax.mean * (1 - exp(-1000)/para$h2.E_star.mean),
        na.rm = T)
    plot(seq(0, 1000, by = 10), para$h2.gmax.mean[1] * (1 - exp(-seq(0,
        1000, by = 10)/para$h2.E_star.mean[1])), ylim = c(0,
        max), type = "l", col = "#00007F", lwd = 2, xlab = "Light Intensity",
        ylab = paste("Gamma (per", 10, "min)"))
    for (i in 1:length(h2.time)) points(seq(0, 1000, by = 10),
        para$h2.gmax.mean[i] * (1 - exp(-seq(0, 1000, by = 10)/para$h2.E_star.mean[i])),
        type = "l", col = jet.colors(nrow(del))[cut(as.numeric(h2.time),
            length(h2.time))][i], lwd = 2)
    ylim <- par("usr")[c(3, 4)]
    xlim <- par("usr")[c(1, 2)]
    color.legend(xlim[2] - diff(xlim)/40, ylim[1], xlim[2], ylim[2],
        legend = format(as.POSIXct(range(h2.time, na.rm = T),
            origin = "1970-01-01"), "%d %b"), rect.col = jet.colors(100),
        gradient = "y", align = "rb")
}
