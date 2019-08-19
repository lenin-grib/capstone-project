library(quanteda)
library(tm)
library(tidytext)
library(dplyr)
library(stringr)


train <- readRDS("merged_train.RDS") ## ~50% of twitter and news, ~20% of blogs

######## splitting set into processible chunks
k=40
trainlist <- split(train, (1:nrow(train) %% (k+1)))

rm(train)

#### for testing
#k=10
#subtrainlist <- split(subtrain, (1:nrow(subtrain) %% (k+1)))

######### METHODS
######### creating and cleaning corpus

cleancorpus <- function(dataset) {
        
        scrub_special_char <- function(x) iconv(x, "UTF-8", "ascii", sub="?")
        
        dataset$data <- scrub_special_char(dataset$data)
        
        trainv <- as.vector(dataset$data)
        corpus <- VCorpus(VectorSource(trainv))
        
        # remove URLS, mentions and hashtags
        toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
        corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
        corpus <- tm_map(corpus, toSpace, "[@#][^\\s]+")
        
        # remove profanity: 
        profanityWords = readLines('profanity_list.txt')
        corpus <- tm_map(corpus,removeWords, profanityWords)
        
        # remove numbers 
        corpus <- tm_map(corpus, content_transformer(removeNumbers))
        
        # remove punctuation
        corpus <- tm_map(corpus, content_transformer(removePunctuation), 
                preserve_intra_word_dashes=FALSE, 
                preserve_intra_word_contractions = TRUE)
        # lower case
        corpus <- tm_map(corpus, content_transformer(tolower)) 
        
        # remove white spaces
        corpus <- tm_map(corpus, stripWhitespace)
        
        # prepare for quanteda
        corpus <- corpus(corpus)
        
        return(corpus)
}

######### creating dataframes from ngrams

makeDF <- function(ng) {
        tmp <- tidy(ng)
        tmp <- as.data.frame(tmp)
        tmp <- tmp %>% 
                group_by(term) %>% 
                summarize(total = sum(count)) %>% 
                select(term, total) %>%
                arrange(desc(total))
        return(tmp)
}

######### join lists (chunks of data) to data frame

joinlist <- function (list){
        x <- c("term", "total")
        freqdf <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(freqdf) <- x
        for (i in 1:k){
                freqdf <- rbind(freqdf, list[[i]])
        }
        freqdf %>% 
                group_by(term) %>% 
                summarize(count = sum(total)) %>% 
                arrange(count)
        return(freqdf)
}

############# gathering frequency data 

process <- function(lst){
        
        unidf <- list()
        bidf<- list()
        tridf <- list()
        quaddf<- list()
        
        for (i in 1:k) {
                corpus <- cleancorpus(lst[[i]])
                ngr1 <- dfm(corpus, ngrams = 1, concatenator = " ")
                ngr2 <- dfm(corpus, ngrams = 2, concatenator = " ")
                ngr3 <- dfm(corpus, ngrams = 3, concatenator = " ")
                ngr4 <- dfm(corpus, ngrams = 4, concatenator = " ")
                
                
                unidf[[i]] <- makeDF(ngr1)
                bidf[[i]] <- makeDF(ngr2)
                tridf[[i]] <- makeDF(ngr3)
                quaddf[[i]] <- makeDF(ngr4)
                
                print(paste0(i/k*100,"% is completed"))
        }
        
        freq1 <- joinlist(unidf)
        freq2 <- joinlist(bidf)
        freq3 <- joinlist(tridf)
        freq4 <- joinlist(quaddf)
        
        saveRDS(freq1, "freq1.RDS")
        saveRDS(freq2, "freq2.RDS")
        saveRDS(freq3, "freq3.RDS")
        saveRDS(freq4, "freq4.RDS")
        
        
}

Rprof(process(trainlist))

###### tetsing
#process(subtrainlist,5)

##### find frequency for the first word
train <- readRDS("merged_train.RDS") ## ~50% of twitter and news, ~20% of blogs

# create smaller sample for first words prediction
set.seed(19082019)
insam <- as.logical(rbinom (n = nrow(train), 1, prob = 0.1))
subtrain <- data.frame(data = train[insam,])

subt <- subtrain %>% 
        mutate(data = word(subtrain$data, 1)) %>%
        select(data)

capFirst <- function(s) {
        paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

corpus1 <- cleancorpus(subt)
ngr1 <- dfm(corpus, ngrams = 1, concatenator = " ", 
        remove = stopwords())
unidfs <- makeDF(ngr1)
stop <- c("rt", "love", "get")
unidftop <- unidfs %>%
        filter(!(term %in% stop)) %>%
        head(10) %>% 
        mutate(term = capFirst(term)) %>%
        print()
saveRDS(unidftop, "freqstart.RDS")

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







