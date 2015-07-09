# Trying to rebuild the bin_comparison script to work more intuitively

library(plyr)
library(tictoc)

tic()
##load your data:
setwd('~/aProjects/FactorTesting/comp_POE/')
frames <- list()
filenames <- list.files()
keeper_list <- list() #initialize the keeper list  #This should be removed to make it more efficient


for (i in 1:length(filenames)){
  frames[[i]] <- read.csv(filenames[i])
}
# Possible improvements: Load in any group of csv's and name them accordingly....use list.files(pattern='*.csv')

# Now create your rankings for each file, one at a time:
setwd('~/aProjects/FactorTesting/') 

for (k in 1:length(frames)){
    
  #Importing the data:
  x <- as.data.frame(frames[[k]], stringsAsFactors = FALSE)
  rownames(x) <- x[,1]
  factor_list <- x[,1]
  x <- x[,c(3,4,5,6)]
  colnames(x) <- c('IC','IC.tstat','IC.sd','F1.excess')
  x
  
  #Need to purge values that would not have been available for that time period
  bad_row <- grep('#N/', x[,1], fixed = TRUE)
  if (length(bad_row)>0){
    x <- x[-bad_row,]                       #Update dataframe
    factor_list <- factor_list[-bad_row]    #Update factor list
  }
  x
  
  #Create an empty DF for the outcomes
  df <- as.data.frame(matrix(nrow=length(factor_list), ncol=length(factor_list)))
  rownames(df) <- factor_list
  colnames(df) <- factor_list
  
  ####################################################################
    
  # Populate the outcome DF
  source('versus2.R')
  for (i in 1:length(factor_list)){
    for (j in 1:length(factor_list)){
      if (i==j) next                        #Skip cases where factor will be against itself
      a <- rownames(df)[i]
      b <- colnames(df)[j]
      df[i,j] <- versus2(x,a,b)
    }
  }
  
  
  # Now count the 'wins'
  master <- as.data.frame(df[upper.tri(as.matrix(df))])       #the upper triangle of the 'versus' matrix
  colnames(master) <- 'V1'
  counts <- ddply(master, .(V1), summarise, V2 = length(V1))  #Count number of times each factor appears on the list
  counts <- counts[order(counts[,'V2'],decreasing = TRUE),]   #Order decreasing
  top <- as.data.frame(counts$V1[c(1:10)])                     #Best (5?) factors
  
  keeper_list[[k]] <- top
}

winners <- do.call(rbind, keeper_list)
colnames(winners) <-'V1'
consistency <- ddply(winners, .(V1), summarise, V2 = length(V1))        #How many times did each factor in the keeper list show up in top 5?
consistency <- as.data.frame(consistency[order(consistency[,'V2'], decreasing=TRUE),])
winners <- as.data.frame(winners[!duplicated(winners),])
toc()


##################################################################################
#More automation:
##################################################################################

factor_list_new <- consistency[!consistency$V2==1,]$V1         #all factors that make into the top 5 more than once

for (i in 1:length(frames)){
  temp <- frames[[i]]
  temp <- temp[temp[,1] %in% factor_list_new,]
  frames[[i]] <- temp
}

#Now run it again with the new frames....Keep going until you've found the best factor





