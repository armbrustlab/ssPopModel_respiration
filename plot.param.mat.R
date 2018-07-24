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

#list.dist  <- list.files("input", "size.distribution_Prochlorococcus",full.names=T)
#load(list.dist[1])
#plot.size.distribution(distribution[[1]][37:61], type='l', lwd=2)


## MERGE MODEL OUTPUT (dt)
list.output <- list.files("output_matrix", "size",full.names=T)

params.all <- NULL

for(path.distribution in list.output){
    #path.distribution <- list.output[1]
    print(path.distribution)
    load(path.distribution)
    file.name <- unlist(list(strsplit(basename(path.distribution), "_")))
    origin <- file.name[1]
    size <- as.numeric(file.name[3])
    dt <- as.numeric(file.name[4])

    # get the parameters
    params <- model1[,2][[1]]
    gr <- model1[,2][[2]]
    # save all the parameters
    temp <- data.frame(origin, size, dt, params,gr=sum(gr,na.rm=T))
    params.all <- data.frame(rbind(params.all, temp))
    # i <- i + 1
}

params.all <- params.all[order(as.numeric(params.all$dt)),]


################ PLOT parameters

# Colors
colfunc <- colorRampPalette(c("red", "blue"))
color <- colfunc(length(params.all$dmax))

# Get the par data
Par <- read.csv("Par.csv")

# get the max delta
max <- max(params.all$gmax * (1 - exp(-1000)/params.all$E_star),
    na.rm = T)

# plot the first delta
plot(seq(0, max(Par$par), by=10),
    params.all$gmax[1] * (1 - exp(-seq(0, max(Par$par), by=10)/params.all$E_star[1])),
    ylim = c(0, max), type = "l", col = "red",
    lwd = 0.1, xlab = "Light Intensity",
    ylab = paste("Gamma (per", 10, "min)"))

# plot the rest of the delta's
for (i in 2:length(params.all$gmax)) {

    #if (i %% 50 != 0) next

    points(seq(0, max(Par$par), by=10),
        params.all$gmax[i] * (1 - exp(-seq(0, max(Par$par), by=10)/params.all$E_star[i])),
        ylim = c(0, max), type = "l", col = color[i],
        lwd = 0.1, xlab = "Light Intensity",
        ylab = paste("Gamma (per", 10, "min)"))


}

# We get the range of the volbin sizes from one of the models,
# the range is the same for all runs of the model
range <- range(as.numeric(row.names(model1[3,][[2]])))

volbins <- seq(range[1], range[2], length.out=100)

delta <- matrix(nrow = length(params.all$gr), ncol = length(volbins))


for (i in 1:length(volbins)) {
    delta[,i] <- params.all$dmax *
        ((volbins[i] / max(volbins))^params.all$b /
        ((1 + volbins[i]/max(volbins))^params.all$b))
}

plot(volbins, delta[1,], ylim=c(0, max(delta, na.rm=T)),type='l', lwd=0.1, col = "red")

for (i in 2:length(params.all$dmax)) {
    points(volbins, delta[i,], ylim=c(0, max(delta, na.rm=T)),type='l', lwd=0.1, col=color[i])
}
