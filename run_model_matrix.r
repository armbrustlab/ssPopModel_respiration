# This function will run the ssPopModel for the given cell distribtion, varying
# this dt between 0 and 30 minutes
# parallel ‘Rscript run_model_matrix.r {}’ ::: {1..69}

args <- commandArgs(TRUE)
distribution.num <- as.numeric(args[1])

# Path to the input distribtions
path.to.git.repository <- "~/ssPopModel_sensitivity_test"
setwd(path.to.git.repository)
Par <- read.csv("Par.csv")
list.dist  <- list.files("input", "size.distribution_Prochlorococcus",full.names=T)

# Get the correct distribtion from the list of distribution
distribtion <- list.dist[distribution_num]
load(distribtion)

# Get the width of the size class
width <- unlist(list(strsplit(list.dist, "_")))[3]

# set the start time to 1, this will not be changed
t <- 1

# Now we will run the ssPopModel for the cell distribtuion, saving
# the output in the format: type.distribtuion_Prochlorococcus_width_dt
for(dt in seq(1, 30, by=0.5)) {
    print(paste0("Time = ", dt))
    freq.distribution2 <- distribution[[2]][37:61] # biomass data for 1 day only
    freq.distribution1 <- distribution[[1]][37:61] # size data for 1 day only
    Ntot <- distribution[[3]][37:61]
    # 1. calculating division rate based on biomass
    model2 <- run.ssPopModel(freq.distribution2, Ntot, Par, time.delay=t, dt=dt)
    save(model2, file=paste0('output/biomass_modeloutput_', width, '_', dt))
    # 2. calculating division rate based on size
    model1 <- run.ssPopModel(freq.distribution1, Ntot, Par, time.delay=t, dt=dt)
    save(model1, file=paste0('output/size_modeloutput_', width, '_', dt))
}

print(paste0("Completed ", distribution_num))
