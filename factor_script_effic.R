# Function for printing out the correct list required as a screen input in FactSet#
###################################################################################

Uscreen <- function(factors = c('ROW12', 'ROW18','ROW9','ROW21','ROW24','ROW26','ROW30','ROW36')){
  vars <- list()
    for (i in 1:length(factors)){
      vars[[i]] <- t(as.matrix(combn(factors,i)))
      vars[[i]] <- as.data.frame(vars[[i]])
    }

  #Create an empty data frame that can be populated with the various factor combinations
  master <- data.frame()

  #Populate the df
  for (i in 1:length(factors)){
    #Different behavior for first (because it should already only be one column in width)
    if(i==1){
      #append it straight to the list
      master <- rbind(vars[[i]],master)
    }
  
    #For other groups:
    else{
      temp <- vars[[i]]
      params <- list()                      #initialize empty parameter list that will be fed to the paste function
      for (j in 1:length(colnames(temp))){  #extract the colnames and populate param list
        params[[j]] <- temp[,j]
      }
      out <- as.data.frame(do.call(paste, c(list(sep=','),params))) #########
      colnames(out) <- 'V1'  
      master <- rbind(master,out)
    }
  }
  factor_list.df <<- within(master, V1 <- paste('AVG(ROW6,ROW7,ROW8,ROW15,',V1,')',sep=''))
  master <- within(master, V1 <- paste('AVG(ROW6,ROW7,ROW8,ROW15,',V1,')',sep=''))
  
  #now separated by ;
  line_list <- list()
  master_t <- t(master)
    for (j in 1:dim(master)[1]){  #extract the colnames and populate param list
    line_list[[j]] <- master_t[,j]
  }
  one_line <- do.call(paste, c(list(sep=';'),line_list))
  one_line
}

