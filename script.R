#### INSTALL ssPopModel
# in the terminal, cd to where you save ssPopModel git repo.
# Then, type in the terminal: R CMD INSTALL ssPopModel
# NB: ssPopModel requires the package DEoptim to be installed, so make sure to install DEoptim package first

library(ssPopModel)


path.to.data <- "~/Documents/Armbrust/ssPopModel_sensitivity_test-master/ssPopModel_sensitivity_test_data"
path.to.git.repository <- "~/Documents/Armbrust/ssPopModel_sensitivity_test-master"

# ## CREATE SIZE DISTRIBUTION
# #1. to download the data (using DAT)
# dat://456f261260e4ae8af7e7dc8b97fdabfdc770f561771e844888b1ef7f59a507ec
#
# #2. calculate size distrubution
# setwd(path.to.data)
# popname <- "prochloro"
# time.interval <- 60 #minutes
# opp.dir <- "SCOPE_6_opp"
# vct.dir <- "SCOPE_6_vct"
# db <- "SCOPE_6.db"
# inst <- "740"
#
# for(width in seq(0.045,0.10,by=0.001)){
#   distribution <- size.distribution(db, opp.dir, vct.dir, popname=popname, volume.width=width, time.interval = time.interval)
#   save(distribution,file=paste0(path.to.git.repository,"/input/size.distribution_Prochlorococcus_",width))
# }



setwd(path.to.git.repository)
Par <- read.csv("Par.csv")


## ESTIMATE GROWTH RATE
list.dist  <- list.files("input", "size.distribution_Prochlorococcus",full.names=T)

for(path.distribution in list.dist){
          #path.distribution <- list.dist[1]
          print(path.distribution)
          load(path.distribution)
          size <- unlist(list(strsplit(path.distribution, "_")))[3]
          freq.distribution2 <- distribution[[2]][37:61] # biomass data for 1 day only
          freq.distribution1 <- distribution[[1]][37:61] # size data for 1 day only

          #plot.size.distribution(freq.distribution2, mode="log", type="l", lwd=2)

          Ntot <- distribution[[3]][37:61]
          t <- 1
            # 1. calculating division rate based on biomass
            model2 <- run.ssPopModel(freq.distribution2, Ntot, Par, time.delay=t, dt=10)
              save(model2, file=paste0('output/biomass_modeloutput_',size))
            # 2. calculating division rate based on size
            model1 <- run.ssPopModel(freq.distribution1, Ntot, Par, time.delay=t, dt=10)
              save(model1, file=paste0('output/size_modeloutput_',size))

}



## Run model varying dt, width at 0.10

list.dist.10 <- list.dist[59]

for(dt in seq(3, 30, by=1)) {
    print(paste0("Time = ", dt))
    load(list.dist.10)
    freq.distribution2 <- distribution[[2]][37:61] # biomass data for 1 day only
    freq.distribution1 <- distribution[[1]][37:61] # size data for 1 day only

    #plot.size.distribution(freq.distribution2, mode="log", type="l", lwd=2)

    Ntot <- distribution[[3]][37:61]
    t <- 1

    # 1. calculating division rate based on biomass
    model2 <- run.ssPopModel(freq.distribution2, Ntot, Par, time.delay=t, dt=dt)
    save(model2, file=paste0('output/biomass_modeloutput_0.10_dt=', dt))
    # 2. calculating division rate based on size
    model1 <- run.ssPopModel(freq.distribution1, Ntot, Par, time.delay=t, dt=dt)
    save(model1, file=paste0('output/size_modeloutput_0.10_dt=', dt))
}

## MERGE MODEL OUTPUT (dt)
list.output <- list.files("output", "dt=",full.names=T)
DF <- NULL



## MERGE MODEL OUTPUT
list.output  <- list.files("output", "modeloutput",full.names=T)
DF <- NULL

for(path.distribution in list.output){
    #path.distribution <- list.output[1]
    print(path.distribution)
    load(path.distribution)
    size <- as.numeric(unlist(list(strsplit(path.distribution, "_")))[3])
    origin <- unlist(list(strsplit(basename(path.distribution), "_")))[1]
    if(origin == "biomass"){
      params <- model2[,2][[1]]
      gr <- model2[,2][[2]]
    }
    if(origin == "size"){
      params <- model1[,2][[1]]
      gr <- model1[,2][[2]]
    }
    df <- data.frame(origin, size, params,gr=sum(gr,na.rm=T))
    DF <- data.frame(rbind(DF, df))
}


## PLOTTING (dt)
par(mfrow=c(3,2))
for(param in colnames(DF)[-c(1:2)]){
    plot.ts(DF[which(DF$origin=="biomass"),param],
    ylim=c(range(DF[,param])),type='p',main=paste(param),ylab=NA, xlab=NA)
    points(DF[which(DF$origin=="size"),param],col=2,type='p')
}



## PLOTTING
par(mfrow=c(3,2),pty='m',cex=1.2)
for(param in colnames(DF)[-c(1:2)]){
    #param <- 'gmax'
    plot(DF[which(DF$origin=="biomass"),"size"], DF[which(DF$origin=="biomass"),param], ylim=c(range(DF[,param])),type='p',main=paste(param),ylab=NA, xlab=NA)
    points(DF[which(DF$origin=="size"),"size"], DF[which(DF$origin=="size"),param],col=2,type='p')
}

par(mfrow=c(1,1),cex=1.2)
plot(DF[which(DF$origin=="biomass"),"size"], abs(DF[which(DF$origin=="biomass"),param]-DF[which(DF$origin=="size"),param]),type='p',main=paste(param),ylab=NA, xlab=NA,cex=2)
