library(ssPopModel)
library(gplots)

#John
path.to.data <- "~/ssPopModel_sensitivity_test/ssPopModel_sensitivity_test_data"
path.to.git.repository <- "~/ssPopModel_sensitivity_test"
setwd(path.to.git.repository)


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


# A few of the outputs were from old runs, this gets rid of them
DF <- na.omit(DF)
# # Some of the GR's came out negative
# DF <- DF[(DF$gr >= 0),]

# Building the matrix to plot
# GR

# Get the number of unique size classes and dt's
size <- sort(unique(DF$size))
dt <- sort(unique(DF$dt))

# this function creates a matrix from the DF table given
# the column index
create.matrix <- function(DF, index) {
    # create the name of the matrix
    parameter <- colnames(DF)[index]
    name <- paste0(parameter, ".matrix")
    # create the empty matrix
    matrix <- matrix(,nrow = length(dt), ncol= length(size))
    index = 1
    # fill in the matrix
    for (i in c(1:length(size))) {
        for (j in c(1:length(dt))) {
            # makes negative entires 0
            if (DF[[parameter]][index] < 0) {
                matrix[j,i] <- 0
            }
            else {
                matrix[j,i] <- DF[[parameter]][index]
            }
            index <- index + 1
        }
    }
    rownames(matrix) <- dt
    colnames(matrix) <- size

    return (matrix)
}

# creates a matrix for each parameter
for (i in c(4:9)) {
    matrix <- create.matrix(DF, i)
    name <- paste0(colnames(DF)[i], ".matrix")
    # rownames(matrix) <- dt
    # colnames(matrix) <- size
    assign(name, matrix)
}

par(mfrow=c(3,2),pty='m')
image(gmax.matrix, xlab="dt", ylab="size")
image(dmax.matrix)
image(b.matrix)
image(E_star.matrix)
image(resnorm.matrix)
image(gr.matrix)
