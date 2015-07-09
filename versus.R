# Need a function that compares values of each together

versus <- function(dataframe,a = 'A',b = 'B'){
  num_vars <- length(colnames(dataframe))
  win <- vector(length = num_vars)
  diff <- vector(length = num_vars)
  scorecard <- data.frame(win, diff)
  for (i in 1:num_vars){
    score_a <- grep(a, dataframe[,i], fixed = TRUE)
    score_b <- grep(b, dataframe[,i], fixed = TRUE)
    if(score_a < score_b){
      scorecard$win[i] <- a
      scorecard$diff[i] <- abs(score_a - score_b)
    }
    if(score_a > score_b){ 
      scorecard$win[i] <- b
      scorecard$diff[i] <- abs(score_a - score_b)
    }
  }
  #Now who is the winner?
  score <- count(scorecard$win)
  
  #Unanimous win
  if (dim(score)[1] == 1) return(as.character(score[1,1]))
  
  #Resolve ties using IC:
  if (score$freq[1] == score$freq[2]){
    return(as.character(scorecard[1,1]))
  }
  
  #If there is not a tie:
  winner <- as.data.frame(score[order(score[,'freq'],decreasing = TRUE),], stringsAsFactors = FALSE)
  as.character(winner[1,1])
}

