# Need a function that compares values of each together better than before

versus2 <- function(dataframe, a, b){
  # Need to compare these factors to each other for each variable
  num_vars <- length(colnames(dataframe))
  
  outcome <- vector(length = num_vars)
  
  for (i in 1:num_vars){
    if (i == 3){
      outcome[i] <- as.numeric(dataframe[a,i]) <= as.numeric(dataframe[b,i])
      next
    }
    outcome[i] <- as.numeric(dataframe[a,i]) >= as.numeric(dataframe[b,i])
  }
  score <- sum(outcome) #Sums the logicals (true =1 and false = 0). If sum > 2 then a wins.
  if (score > num_vars/2) winner <- a
  if (score < num_vars/2) winner <- b
  if (score == num_vars/2){           # In the event of a tie, use the IC as a tiebreaker
    if (outcome[1] == TRUE) winner <- a
    if (outcome[1] == FALSE) winner <- b
  }
  
  winner
}