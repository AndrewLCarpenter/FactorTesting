# Try to automate this
library(plyr)
library(tictoc)

tic()
##load your data:
setwd('~/aProjects/FactorTesting/doit/')
frames <- list()
filenames <- list.files()
keeper_list <- list() #initialize the keeper list


for (i in 1:length(filenames)){
 frames[[i]] <- read.csv(filenames[i])
}
# Possible improvements: Load in any group of csv's and name them accordingly....use list.files(pattern='*.csv')




# Now create your rankings for each file, one at a time:
setwd('~/aProjects/FactorTesting/') 

for (k in 1:length(frames)){
  
  #Importing the data:
  x <- as.data.frame(frames[[k]], stringsAsFactors = FALSE)
  rownames(x) <- x$Factor
  factor_list <- x$Factor
  x <- x[,c(3,4,5,6)]
  colnames(x) <- c('IC','IC.tstat','IC.sd','F1.excess')
  
  #Need to purge values that would not have been available for that time period
  bad_row <- grep('#N/', x[,1], fixed = TRUE)
  if (length(bad_row)>0){
    x <- x[-bad_row,]
    factor_list <- factor_list[-bad_row]
  }
  
  #Sort it by each column and extract the row names after each sorting.
  for (i in 1:length(x)){
    temp <- rownames(x[order(x[,i],decreasing = TRUE),])
    x[,i] <- temp
  }
  rownames(x) <- c(1:dim(x)[1])
  #Now you have a dataframe with all of the rankings based on each category
  
  
  #Create an empty DF for the outcomes
  df <- as.data.frame(matrix(nrow=length(factor_list), ncol=length(factor_list)))
  rownames(df) <- factor_list
  colnames(df) <- factor_list
  
  ####################################################################
  
  
  # This will populate the outcome DF
  source('versus.R')
  for (i in 1:length(factor_list)){
    for (j in 1:length(factor_list)){
      if (i==j) next
      a <-rownames(df)[i]
      b <- colnames(df)[j]
      df[i,j] <- versus(x,a,b)
    }
  }
  
  
  # Now count the 'wins'
  master <- data.frame()  #empty DF
  
  colnames(df) <- c(rep('V1', dim(df)[2])) #formatting issues when combining dataframes. (Fix this?)
  
  for(i in 1:dim(df)[2]){
    temp <- as.data.frame(df[,i])
    colnames(temp) <- 'V1'
    master <- rbind(master,temp)
    colnames(master) <- 'V1'
  }
  
  counts <- ddply(master, .(V1), summarise, V2 = length(V1))  #Count number of times each factor appears on the list
  counts <- counts[order(counts[,'V2'],decreasing = TRUE),]   #Order decreasing
  top10 <- as.data.frame(counts$V1[c(1:5)])
  
  keeper_list[[k]] <- top10
}

winners <- do.call(rbind, keeper_list)
colnames(winners) <-'V1'
q <- ddply(winners, .(V1), summarise, V2 = length(V1))
q <- q[order(q[,'V2'], decreasing=TRUE),]
winners <- as.data.frame(winners[!duplicated(winners),])
toc()

old_w <-winners
old_q <- q

