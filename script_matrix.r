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

    if(origin == "biomass"){
        params <- model2[,2][[1]]
        gr <- model2[,2][[2]]
    }
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

# Make a 59x69 matrix, for the (size)x(dt) data
gr.matrix <- matrix(,nrow = length(dt), ncol = length(size))
rownames(gr.matrix) <- dt
colnames(gr.matrix) <- size

# Build the matrix
index = 1
for (i in c(1:length(size))) {
    for (j in c(1:length(dt))) {
        if (DF$gr[index] < 0) {
            gr.matrix[j,i] <- 0
        }
        else {
            gr.matrix[j,i] <- DF$gr[index]
        }
        index <- index + 1
    }
}

# Resnorm
res.matrix <- matrix(,nrow = length(dt), ncol = length(size))
rownames(res.matrix) <- dt
colnames(res.matrix) <- size

# Build the matrix
index = 1
for (i in c(1:length(size))) {
    for (j in c(1:length(dt))) {
        if (DF$gr[index] < 0) {
            res.matrix[j,i] <- 0
        }
        else {
            res.matrix[j,i] <- DF$resnorm[index]
        }
        index <- index + 1
    }
}


my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)

#
# # Build the heatmap
# res.heatmap <- heatmap.2(res.matrix, Rowv=NA, Colv=NA, col = my_palette, scale="column", margins=c(5,10))
#
# # Build the heatmap
# gr.heatmap <- heatmap.2(gr.matrix, Rowv=NA, Colv=NA, col = my_palette, scale="column", margins=c(5,10))
library(plotly)
heatmap <- plot_ly(z = res.matrix, type = "heatmap", colors = c("black", "white"))
heatmap
surface <- plot_ly(z = ~gr.matrix, xlab="width", ylab="dt") %>% add_surface()
surface


image(gr.matrix)
image(res.matrix)
