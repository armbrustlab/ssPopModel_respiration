#### INSTALL ssPopModel
# in the terminal, cd to where you save ssPopModel git repo.
# Then, type in the terminal: R CMD INSTALL ssPopModel
# NB: ssPopModel requires the package DEoptim to be installed, so make sure to install DEoptim package first


library(ssPopModel)

path.to.data <- "~/Desktop/model_test/"

setwd(path.to.data)
Par <- read.csv("Par.csv")

list.dist  <- list.files(".", "size.distribution_Prochlorococcus")


## ESTIMATE GROWTH RATE
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
              save(model2, file=paste0('biomass_modeloutput_',size))
            # 2. calculating division rate based on size
            model1 <- run.ssPopModel(freq.distribution1, Ntot, Par, time.delay=t, dt=10)
              save(model1, file=paste0('size_modeloutput_',size))

}





## MERGE MODEL OUTPUT
list.output  <- list.files(".", "modeloutput")
DF <- NULL
for(path.distribution in list.output){
    #path.distribution <- list.output[1]
    print(path.distribution)
    load(path.distribution)
    size <- as.numeric(unlist(list(strsplit(path.distribution, "_")))[3])
    origin <- unlist(list(strsplit(path.distribution, "_")))[1]
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




## PLOTTING
par(mfrow=c(3,2))
for(param in colnames(DF)[-c(1:2)]){
    #param <- 'gmax'
    plot(DF[1:23,"size"], DF[1:23,param], ylim=c(range(DF[,param])),type='o',main=paste(param),ylab=NA, xlab=NA)
    points(DF[24:46,"size"], DF[24:46,param],col=2,type='o')
}

plot(DF[1:23,"size"], DF[1:23,param]-DF[24:46,param],type='o',main=paste(param),ylab=NA, xlab=NA)
abline(h=0)
