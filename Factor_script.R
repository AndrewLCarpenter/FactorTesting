install.packages('combinat')
library(combinat)

factors = c('%Tgt Avg','%Tgt+3', 'Fin Sust', 'RONA', 'EM Mo', 'P/52 Wk Hi', '6m Ret','AFG Co')
factors = c('ROW1', 'ROW2', 'ROW3', 'ROW4', 'ROW5', 'ROW6', 'ROW7', 'ROW8')
factors = c(LETTERS[1:8])

vars <- list()
for (i in 1:8){
  vars[[i]] <- t(as.matrix(combn(factors,i)))
}

one <- as.data.frame(vars[[1]])
one <- within(one, V1 <- paste(V1,sep=','))
one

two <- as.data.frame(vars[[2]])
two <- within(two, V1 <- paste(V1,V2,sep=','))
two <- as.data.frame(two$V1)
colnames(two) = 'V1'

three <- as.data.frame(vars[[3]])
three <- within(three, V1 <- paste(V1,V2,V3,sep=','))
three <- as.data.frame(three$V1)
colnames(three) = 'V1'

four <- as.data.frame(vars[[4]])
four <- within(four, V1 <- paste(V1,V2,V3,V4,sep=','))
four <- as.data.frame(four$V1)
colnames(four) = 'V1'

five <- as.data.frame(vars[[5]])
five <- within(five, V1 <- paste(V1,V2,V3,V4,V5,sep=','))
five <- as.data.frame(five$V1)
colnames(five) = 'V1'

six <- as.data.frame(vars[[6]])
six <- within(six, V1 <- paste(V1,V2,V3,V4,V5,V6,sep=','))
six <- as.data.frame(six$V1)
colnames(six) = 'V1'

seven <- as.data.frame(vars[[7]])
seven <- within(seven, V1 <- paste(V1,V2,V3,V4,V5,V6,V7,sep=','))
seven <- as.data.frame(seven$V1)
colnames(seven) = 'V1'

eight <- as.data.frame(vars[[8]])
eight <- within(eight, V1 <- paste(V1,V2,V3,V4,V5,V6,V7,V8,sep=','))
eight <- as.data.frame(eight$V1)
colnames(eight) = 'V1'

factor_list <- rbind(one,two,three,four,five,six,seven,eight)
factor_list <- within(factor_list, V1 <- paste('AVG(',V1,')',sep=''))
factor_list

