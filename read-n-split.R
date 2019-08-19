filesen <- c("./final/en_US/en_US.twitter.txt", 
        "./final/en_US/en_US.news.txt",
        "./final/en_US/en_US.blogs.txt")
source("methods.R")

set.seed(19082019)

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



