# This function will run the ssPopModel for the given cell distribtion, varying
# this dt between 0 and 30 minutes

run_model_matrix <- function(distribution_num) {

    # Path to the input distribtions
    path.to.git.repository <- "~/ssPopModel_sensitivity_test"
    setwd(path.to.git.repository)
    list.dist  <- list.files("input", "size.distribution_Prochlorococcus",full.names=T)

    # Get the correct distribtion from the list of distribution
    distribtion <- list.dist[distribution_num]
    load(dist)

    # set the start time to 1, this will not be changed
    t <- 1

    # Now we will run the ssPopModel for the cell distribtuion, saving
    # the output in the format: type.distribtuion_Prochlorococcus_size_dt
    for(dt in seq(1, 30, by=0.25)) {
        print(paste0("Time = ", dt))
        freq.distribution2 <- distribution[[2]][37:61] # biomass data for 1 day only
        freq.distribution1 <- distribution[[1]][37:61] # size data for 1 day only
        Ntot <- distribution[[3]][37:61]
        # 1. calculating division rate based on biomass
        model2 <- run.ssPopModel(freq.distribution2, Ntot, Par, time.delay=t, dt=dt)
        save(model2, file=paste0('output/biomass_modeloutput_0.10_dt=', dt))
        # 2. calculating division rate based on size
        model1 <- run.ssPopModel(freq.distribution1, Ntot, Par, time.delay=t, dt=dt)
        save(model1, file=paste0('output/size_modeloutput_0.10_dt=', dt))
    }

    print(paste0("Completed " distribution))
}
