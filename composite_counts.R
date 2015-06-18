#This function takes in the output from an alpha test and ranks the factors based on the number of times the perform in the top 50
factorCount <- function(file = '~/aProjects/FactorTesting/top50.csv', cutoff=49){
  
library(plyr)

df <- read.csv(file)
df <- as.data.frame(df, stringsAsFactors = FALSE)
df <- df[c(1:cutoff),]

master <- data.frame()

colnames(df) <- c(rep('V1', dim(df)[2]))

for(i in 1:dim(df)[2]){
  temp <- as.data.frame(df[,i])
  colnames(temp) <- 'V1'
  master <- rbind(master,temp)
  colnames(master) <- 'V1'
}

counts <- ddply(master, .(V1),summarise, V2 = length(V1))
counts <- counts[order(counts[,'V2'],decreasing = TRUE),]
assign(paste0('factors_',as.character(cutoff)),counts, envir = .GlobalEnv)
counts
}

