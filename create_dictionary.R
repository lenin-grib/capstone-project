source("methods.R")

train <- readRDS("merged_train.RDS") ## ~50% of twitter and news, ~20% of blogs

######## splitting set into processible chunks
k=40
trainlist <- split(train, (1:nrow(train) %% (k+1)))
rm(train)

## create dictionary with stopwords included
process(trainlist)


rm(list=ls())

###########################################







