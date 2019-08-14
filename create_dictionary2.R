train <- readRDS("merged_train.RDS") ## 60% of twitter and news, 30% of blogs
str(train)

######## splitting set into processible chunks
k=40
trainlist <- split(train, (1:nrow(train) %% (k+1)))
rm(train)

######### creating and cleaning corpus
library(tm)

cleancorpus <- function(data) {
        trainv <- as.vector(data$data)
        corpus <- VCorpus(VectorSource(trainv))
        
        # remove URLS, mentions and hashtags
        toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
        corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
        corpus <- tm_map(corpus, toSpace, "[@#][^\\s]+")
        
        # remove numbers 
        corpus <- tm_map(corpus, content_transformer(removeNumbers))
        
        # remove punctuation
        corpus <- tm_map(corpus, content_transformer(removePunctuation), 
                preserve_intra_word_dashes=FALSE, 
                preserve_intra_word_contractions = TRUE)
        # lower case
        corpus <- tm_map(corpus, content_transformer(tolower)) 
        
        # remove profanity: 
        profanityWords = readLines('profanity_list.txt')
        corpus <- tm_map(corpus,removeWords, profanityWords)
        
        # remove white spaces
        corpus <- tm_map(corpus, stripWhitespace) 
        
        return(corpus)
}

######### creating ngrams

library(RWeka)

Unigram <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
Bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
Trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
Quadgram <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

makeDF <- function(ng) {
        tmp <- tidy(ng)
        tmp <- as.data.frame(tmp)
        tmp <- tmp %>% 
                group_by(term) %>% 
                summarize(total = sum(count)) %>% 
                select(term, total) %>%
                arrange(total)
        return(tmp)
}



#k=20
#n = floor(length(trainv)/k)

############# gathering frequency data 
library(tidytext)
library(dplyr)

joinlists <- function (list){
        freqdf <- data.frame("term", "count")
        for (i in 1:k){
                freqdf <- rbind(freqdf, list[[i]])
        }
        freqdf %>% 
                group_by(term) %>% 
                summarize(count = sum(total)) %>% 
                arrange(count)
        return(freqdf)
}

process <- function(list){
        
        unidf <- list()
        bidf<- list()
        tridf <- list()
        quaddf<- list()
        
        for (i in 1:k) {
                corpus <- cleancorpus(trainlist[[i]])
                ngr1 <- removeSparseTerms(TermDocumentMatrix(corpus, 
                        control = list(tokenize = Unigram)), 0.9)
                ngr2 <- TermDocumentMatrix(corpus, 
                        control = list(tokenize = Bigram))
                ngr3 <- TermDocumentMatrix(corpus, 
                        control = list(tokenize = Trigram))
                ngr4 <- TermDocumentMatrix(corpus, 
                        control = list(tokenize = Quadgram))
                
                
                unidf[[i]] <- makeDF(ngr1)
                bidf[[i]] <- makeDF(ngr2)
                tridf[[i]] <- makeDF(ngr3)
                quaddf[[i]] <- makeDF(ngr4)
                
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





