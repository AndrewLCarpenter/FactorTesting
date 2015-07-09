library(plyr)

#Importing the data

x <- read.csv('test.csv')
x <- x[c(1:264),]
rownames(x) <- x$Factor
factor_list <- x$Factor
x <- x[,c(3,4,5,17)]
colnames(x) <- c('IC','t.stat','f1.return','f1.excess')

#Create the dataframe, sort it by each column and extract the row names after each sorting.
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


# Now lets count the 'votes'


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








