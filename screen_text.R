#Function that takes in data frame with one row and returns Universal screen recognizable text (for factset US)

screen_text <- function(dataframe){

  line_list <- list()
  transpose <- t(dataframe)
  for (j in 1:dim(dataframe)[1]){  #extract the colnames and populate param list
    line_list[[j]] <- transpose[,j]
  }
  one_line <- do.call(paste, c(list(sep=';'),line_list))
  one_line
}