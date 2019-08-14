library(RWeka)
library(tm)
library(tidytext)
library(dplyr)
library(stringr)


train <- readRDS("merged_train.RDS") ## 60% of twitter and news, 30% of blogs
str(train)

### create smaller sample for first words prediction and PoC
set.seed(14082019)
insam <- as.logical(rbinom (n = nrow(train), 1, prob = 0.05))
subtrain <- data.frame(data = train[insam,])

######## splitting set into processible chunks
k=40
trainlist <- split(train, (1:nrow(train) %% (k+1)))
rm(train)

######### creating and cleaning corpus

cleancorpus <- function(dataset) {
        
        scrub_special_char <- function(x) iconv(x, to="ASCII", sub="?")
        
        dataset$data <- scrub_special_char(dataset$data)
        
        trainv <- as.vector(dataset$data)
        corpus <- VCorpus(VectorSource(trainv))
        
        # remove URLS, mentions and hashtags
        toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
        corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
        corpus <- tm_map(corpus, toSpace, "[@#][^\\s]+")
        
        # remove stopwords
        corpus <- tm_map(corpus, removeWords, stopwords("english"))
        
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
        
        return(corpus)
}

######### creating ngrams


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
                arrange(desc(total))
        return(tmp)
}



#k=20
#n = floor(length(trainv)/k)

############# gathering frequency data 

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
                
                print(paste0(i/k*100,"% is completed"))
        }
        
        freq1 <- joinlist(unidf)
        freq2 <- joinlist(bidf)
        freq3 <- joinlist(tridf)
        freq4 <- joinlist(quaddf)
        
        saveRDS(freq1, "freq1c.RDS")
        saveRDS(freq2, "freq2c.RDS")
        saveRDS(freq3, "freq3c.RDS")
        saveRDS(freq4, "freq4c.RDS")
        
        
}

process(trainlist)

##### find frequency for the first word

subt <- subtrain %>% 
        mutate(data = word(subtrain$data, 1)) %>%
        select(data)

capFirst <- function(s) {
        paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}


corpus1 <- cleancorpus(subt)
ngr1 <- TermDocumentMatrix(corpus1, 
        control = list(tokenize = Unigram))
#inspect(ngr1)
unidf <- makeDF(ngr1)
unidftop <- unidf %>%
        head(10) %>% 
        mutate(term = capFirst(term)) %>%
        print()
saveRDS(unidftop, "freqstartc.RDS")


###########################################







