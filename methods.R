library(quanteda)
library(tm)
library(tidytext)
library(dplyr)
library(stringr)
library(reader)
library(caret)
library(tidyr)
library(DescTools)

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
        
        con <- file(filename, "r") 
        if (nlines != 0) {
                textset <- readLines(con, nlines, encoding = "UTF-8", skipNul = TRUE) 
        } else {
                textset <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
        }
        close(con)
        
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


######### join lists (chunks of data) to data frame

joinlist <- function (list){
        x <- c("term", "total")
        freqdf <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(freqdf) <- x
        for (i in 1:k){
                freqdf <- rbind(freqdf, list[[i]])
        }
        freqdf <- freqdf %>% 
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
        
        if (!stop){
                bidf<- list()
                tridf <- list()
                quaddf<- list()
                pendf <- list()
        }
        
        for (i in 1:k) {
                corpus <- cleancorpus(lst[[i]])
                
                ngr1 <- custngram(corpus,1, stop)
                if (!stop){
                        ngr2 <- custngram(corpus,2, stop)
                        ngr3 <- custngram(corpus,3, stop)
                        ngr4 <- custngram(corpus,4, stop)
                        ngr5 <- custngram(corpus,5, stop)
                }
                
                unidf[[i]] <- makeDF(ngr1)
                if (!stop){
                        bidf[[i]] <- makeDF(ngr2)
                        tridf[[i]] <- makeDF(ngr3)
                        quaddf[[i]] <- makeDF(ngr4)
                        pendf[[i]] <- makeDF(ngr5)
                }
                
                print(paste0(i/k*100,"% is completed"))
        }
        
        rm(corpus)
        
        postfix <- if (stop) "stop_removed" else ""
        
        freq1 <- joinlist(unidf)
        saveRDS(freq1, paste0("freq1",postfix,".RDS"))
        print("1gram created")
        rm(unidf,freq1)
        
        if (!stop){
                freq2 <- joinlist(bidf)
                saveRDS(freq2, "freq2.RDS")
                print("2gram created")
                rm(bidf,freq2)
                
                freq3 <- joinlist(tridf)
                saveRDS(freq3, "freq3.RDS")
                print("3gram created")
                rm(tridf,freq3)
                
                freq4 <- joinlist(quaddf)
                saveRDS(freq4, "freq4.RDS")
                print("4gram created")
                rm(quaddf,freq4)
                
                freq5 <- joinlist(pendf)
                saveRDS(freq5, "freq5.RDS")
                print("5gram created")
                rm(pendf,freq5)
        }
        
}
### tmp
process5 <- function(lst, stop){
        
        pendf <- list()
        unidf <- list()
        for (i in 1:k) {
                corpus <- cleancorpus(lst[[i]])
                ngr1 <- custngram(corpus,1, TRUE)
                ngr5 <- custngram(corpus,5, stop)
                unidf[[i]] <- makeDF(ngr1)
                pendf[[i]] <- makeDF(ngr5)
                print(paste0(i/k*100,"% is completed"))
        }
        
        rm(corpus)
        freq1 <- joinlist(unidf)
        freq5 <- joinlist(pendf)
        saveRDS(freq1, "freq1stopword_removed.RDS")
        print("1gram created")
        saveRDS(freq5, "freq5.RDS")
        print("5gram created")
        rm(pendf,freq5, unidf,freq1)
}

#### arrange the frequncy table
preparefreq <- function(df,n){
        if (n==1){
                df <- df %>%
                        mutate(end = term, start = "", 
                                prob = total/sum(total)) %>%
                        filter(total > 1) %>%
                        head(10) %>%
                        select(start,end, total, prob)  
        } else {
                df <- df %>%
                        mutate(prob = total/sum(total)) %>%
                        filter(total > 1) %>%
                        separate_last(n) 
        }
}


#### extract last words into separate column
separate_last <- function(df, n){
        if (n==2) {
                df <- df %>%
                        separate(term, 
                                into = c("start", "end"), sep = " ")
        } else {
                if (n==3)
                {
                        df <- df %>%
                                separate(term, 
                                        into = c("start1", "start2","end"),
                                        sep = " ") %>%
                                unite("start", c("start1", "start2"), 
                                        sep = " " )
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
                        } else {
                                if (n == 5)
                                {
                                        df <- df %>%
                                                separate(term, 
                                                        into = c("start1", 
                                                                "start2", 
                                                                "start3", 
                                                                "start4", 
                                                                "end"),
                                                        sep = " ") %>%
                                                unite("start", 
                                                        c("start1","start2",
                                                                "start3",
                                                                "start4"), 
                                                        sep = " " )      
                                }
                        }
                }
        }
        return(df)
        
}

#### return last N words from the string
return_last <- function(str, n){
        res <- NULL
        a<-tail(strsplit(str, split=" ")[[1]],n," ")
        for (i in 1:n){
                res<- StrTrim(paste(res, a[i]), 
                        pattern = " \t\n", method = "both")
        }
        return(res)
}

####### simplest prediction using not weighted backoff algorithm
simplepred4 <- function(str,w = c(1,1,1,1), res = NULL){
        
        str <- tolower(str)
        str <- gsub('[[:punct:] ]+',' ',str)
        lng <- str_count(str,"\\S+")
        
        if (lng == 0){
                res <- freqstart %>%
                        head(3)
        }
        
        if (lng>3) {
                str<-return_last(str,3)
                lng <- str_count(str,"\\S+")
        }
        
        if (lng == 3){
                res <- freq4 %>%
                        filter(start == str) %>%
                        mutate(wprob = prob*w[4]) %>%
                        arrange(-wprob) %>%
                        head(5)
                
                str <- return_last(str, 2)
                lng <- str_count(str,"\\S+")
        }
        
        if (lng == 2){
                rest <- freq3 %>%
                        filter(start == str) %>%
                        mutate(wprob = prob*w[3]) %>%
                        arrange(-total) %>%
                        head(5)
                res <- rbind(res,rest)
                str <- return_last(str, 1)
                lng <- str_count(str,"\\S+")
        }
        
        if (lng == 1){
                rest <- freq2 %>%
                        filter(start == str) %>%
                        mutate(wprob = prob*w[2]) %>%
                        arrange(-total) %>%
                        head(5)
                res <- rbind(res,rest)
                str <- ""
                lng <- str_count(str,"\\S+")
        }
        
        if (lng == 0){
                res1 <- freq1 %>%
                        mutate(wprob = prob*w[1]) %>%
                        head(2)
                res2 <- freq1sw %>%
                        mutate(wprob = prob*w[1]) %>%
                        head(3)
                rest <- rbind(res1,res2)
                res <- rbind(res,rest)
        }
        
        if (nrow(res) < 3){
                res1 <- freq1 %>%
                        mutate(wprob = prob*w[1]) %>%
                        head(1)
                res2 <- freq1sw %>%
                        mutate(wprob = prob*w[1]) %>%
                        head(1)
                rest <- rbind(res1,res2)
                res <- rbind(res,rest)
        }
        if (w[1] != w[4]){
                res <- arrange(res, -wprob)
        }
        print(res)
        return(head(res$end,3))
}

simplepred5 <- function(str,w = c(1,1,1,1,1)){
        str <- tolower(str)
        str <- gsub('[[:punct:] ]+',' ',str)
        lng <- str_count(str,"\\S+")
        res5 = NULL
        
        if (lng>4) {
                str<-return_last(str,4)
                lng <- str_count(str,"\\S+")
        }
        
        if (lng == 5){
                res5 <- freq5 %>%
                        filter(start == str) %>%
                        mutate(wprob = prob*w[5]) %>%
                        arrange(-wprob) %>%
                        head(5)
        }
        resturn(simplepred4(str,w[1:4],res5))
        
}

