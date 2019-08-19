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
Rprof(process(trainlist, FALSE))

## create dictionary with stopwords filtered
Rprof(process(trainlist, TRUE))

rm(list=ls())

######### end of main functionality

######### optimizing frequency tables

freq1 <- readRDS(file = "freq1.RDS")

#freq1s <- freq1 %>%
#        group_by(term) %>%
#        summarize(total = sum(total)) %>% 
#        select(term, total) %>%
#        arrange(desc(total))

#saveRDS(freq1s, "freq1s.RDS")
        

freq2 <- readRDS(file = "freq2.RDS")
freq3 <- readRDS(file = "freq3.RDS")
freq4 <- readRDS(file = "freq4.RDS")

cutsparse <- function (df, fn){
        
        df %>%
                filter(total >1) %>%
                saveRDS(fn)
}

cutsparse(freq1,"freq1s.RDS")
cutsparse(freq2,"freq2s.RDS")
cutsparse(freq3,"freq3s.RDS")
cutsparse(freq4,"freq4s.RDS")


###########################################







