factor_list <- c('A','B','C')

df <- as.data.frame(matrix(nrow=length(factor_list), ncol=length(factor_list)))
  rownames(df) <- factor_list
  colnames(df) <- factor_list
  df

# I need a matrix that has all of the results for each metric

####################################################################
#testing
IC <- c(1,3,2)
t_stat <- c(2,4,3)
return <- c(2,5,4)

x <- data.frame(IC,t_stat,return)
rownames(x) <- factor_list
#Create the dataframe, sort it by each column and extract the row names after each sorting. Duh.
x<- new
for (i in 1:length(x)){
  temp <- rownames(x[order(x[,i],decreasing = TRUE),])
  x[,i] <- temp
}
rownames(x) <- c(1:dim(x)[1])
#Now you have a dataframe with all of the rankings based on each category
x$Return


####################################################################


for (i in 1:length){
  for (j in 1:length){
    temp_score1 <- grep(rownames(df)[i], sorted_data)
  }
}

















