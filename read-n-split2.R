filesen <- c("./final/en_US/en_US.twitter.txt", 
        "./final/en_US/en_US.news.txt",
        "./final/en_US/en_US.blogs.txt")

library(dplyr)
library(reader)
library(caret)
library(dplyr)

remove_outliers <- function(data) {
        first <- quantile(nchar(data), .15)
        third <- quantile(nchar(data), .85)
        data <- data[nchar(data) < third]
        data <- data[nchar(data) > first]
        return(data)
}

readnsplit <- function(filename, name, nlines = 0) { 
        
        if (nlines != 0) {
                textset <- n.readLines(filename, skip=0,n = nlines) ## read 10000 lines
        } else {
                con <- file(filename, "r") 
                textset <- readLines(con, encoding = "UTF-8", skipNul = TRUE) ## read the file
                close(con) 
        }
        
        textset <- remove_outliers(textset)
        textset <- as.data.frame(textset)
        
        nr = nrow(textset)
        
        inTrain <- rbinom (n = nr, 1, prob = 0.8)
        test <- data.frame(data = textset[!(inTrain),])
        traintmp <- data.frame(data = textset[as.logical(inTrain),])
        
        rm(textset)
        
        inValid <- as.logical(rbinom 
                (n = floor(nr*0.8), 1, prob = 0.25))
        validation <- data.frame(data = traintmp[as.logical(inValid),])
        train <- data.frame(data = traintmp[!(inValid),])
        
        train <- train %>% mutate(data = as.character(data))
        test <- test %>% mutate(data = as.character(data))
        validation <- validation %>% mutate(data = as.character(data))
        
        saveRDS(train, paste0(name, "_train.RDS"))
        saveRDS(test, paste0(name, "_test.RDS"))
        saveRDS(validation, paste0(name, "_valid.RDS"))
        
        rm(list = ls())
}

set.seed(14082019)

readnsplit(filesen[1], "twi")
readnsplit(filesen[2], "news")
readnsplit(filesen[3], "blogs", 400000)

### prepare train set
news_train <- readRDS("news_train.RDS")
twi_train <- readRDS("twi_train.RDS")
blogs_train <- readRDS("blogs_train.RDS")

train <- rbind(news_train, twi_train, blogs_train)
sum(is.na(train$data))
train<- na.omit(train)

saveRDS(train, "merged_train.RDS")

#### prepare test set
news_test <- readRDS("news_test.RDS")
twi_test <- readRDS("twi_test.RDS")
blogs_test <- readRDS("blogs_test.RDS")

test <- rbind(news_test, twi_test, blogs_test)

sum(is.na(test$data))
#test<- na.omit(test)

saveRDS(test, "merged_test.RDS")

#### prepare validation set
news_valid <- readRDS("news_valid.RDS")
twi_valid <- readRDS("twi_valid.RDS")
blogs_valid <- readRDS("blogs_valid.RDS")

valid <- rbind(news_valid, twi_valid, blogs_valid)
sum(is.na(valid$data))
valid <- na.omit(valid)

saveRDS(valid, "merged_valid.RDS")

rm(list = ls())
##########################################################



