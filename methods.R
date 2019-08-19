library(quanteda)
library(tm)
library(tidytext)
library(dplyr)
library(stringr)
library(reader)
library(caret)
library(tidyr)

###### remove very short and very long entries

remove_outliers <- function(data) {
        first <- quantile(nchar(data), .15)
        third <- quantile(nchar(data), .85)
        data <- data[nchar(data) < third]
        data <- data[nchar(data) > first]
        return(data)
}

###### read data and split into train, test and validation

readnsplit <- function(filename, name, nlines = 0) { 
        
        if (nlines != 0) {
                con <- file(filename, "r") 
                textset <- readLines(con, nlines, encoding = "UTF-8", skipNul = TRUE) ## read the file
                close(con) ## read 10000 lines
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

shapefreq <- function(df){
        df <- df %>%
                group_by(term) %>% 
                summarize(total = sum(total)) %>% 
                select(term, total) %>%
                arrange(desc(total))
        return(df)
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

###### create ngrams with and without filtering stopwords

custngram <- function(corpus, ngr, stop = FALSE){
        if (!stop) {
                custngr <- dfm(corpus, ngrams = ngr, 
                        concatenator = " ") 
        } else {
                custngr <- dfm(corpus, ngrams = ngr, 
                        concatenator = " ", remove = stopwords()) 
        }
        return(custngr)
}

############# gathering frequency data 

process <- function(lst, stop){
        
        unidf <- list()
        bidf<- list()
        tridf <- list()
        quaddf<- list()
        
        for (i in 1:k) {
                corpus <- cleancorpus(lst[[i]])
                
                ngr1 <- custngram(corpus,1, stop)
                ngr2 <- custngram(corpus,2, stop)
                ngr3 <- custngram(corpus,3, stop)
                ngr4 <- custngram(corpus,4, stop)
                
                unidf[[i]] <- makeDF(ngr1)
                bidf[[i]] <- makeDF(ngr2)
                tridf[[i]] <- makeDF(ngr3)
                quaddf[[i]] <- makeDF(ngr4)
                
                print(paste0(i/k*100,"% is completed"))
        }
        
        rm(corpus)
        
        postfix <- if (stop) "stop_removed" else ""
        
        freq1 <- joinlist(unidf)
        freq1 <- shapefreq(freq1)
        saveRDS(freq1, paste0("freq1",postfix,".RDS"))
        print("1gram created")
        rm(unidf,freq1)
        
        freq2 <- joinlist(bidf)
        freq2 <- shapefreq(freq2)
        saveRDS(freq2, paste0("freq2",postfix,".RDS"))
        print("2gram created")
        rm(bidf,freq2)
        
        freq3 <- joinlist(tridf)
        freq3 <- shapefreq(freq3)
        saveRDS(freq3, paste0("freq3",postfix,".RDS"))
        print("3gram created")
        rm(triidf,freq3)
        
        freq4 <- joinlist(quaddf)
        freq4 <- shapefreq(freq4)
        saveRDS(freq4, paste0("freq4",postfix,".RDS"))
        print("4gram created")
        rm(quadf,freq4)
        
}

##### cut terms that are ily met once and save to file
#cutsparse <- function (df, fn){
 #       
 #       df %>%
 #               filter(total >1) %>%
 #               saveRDS(fn)
#}


##### group by starting phrase
group_prob <- function(df){
        df <- df %>%
                arrange(start, total) #%>%
               # mutate(start = as.factor(start))
        return(df)
}

#### extract last words into separate column
separate_last <- function(df, n){
        if (n==2) {
                df <- df %>%
                        separate(term, 
                                into = c("start", "end"), sep = " ") %>%
                        group_prob
        } else {
                if (n==3)
                {
                        df <- df %>%
                                separate(term, 
                                        into = c("start1", "start2","end"),
                                        sep = " ") %>%
                                unite("start", c("start1", "start2"), 
                                        sep = " " ) %>%
                                group_prob
                } else {
                        if (n==4)
                        {
                                df <- df %>%
                                        separate(term, 
                                                into = c("start1", "start2", 
                                                        "start3", "end"),
                                                sep = " ") %>%
                                        unite("start", c("start1", "start2", 
                                                "start3"), 
                                                sep = " " )
                        }
                }
        }
        return(df)
        
}