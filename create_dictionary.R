source("methods.R")

train <- readRDS("merged_train.RDS") ## ~50% of twitter and news, ~20% of blogs

######## splitting set into processible chunks
k=40
trainlist <- split(train, (1:nrow(train) %% (k+1)))
rm(train)

#### for testing
#k=10
#subtrainlist <- split(subtrain, (1:nrow(subtrain) %% (k+1)))

## create dictionary with stopwords included
process(trainlist, FALSE)

## create dictionary with stopwords filtered
process(trainlist, TRUE)

rm(list=ls())

###########################################







