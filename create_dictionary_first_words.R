source("methods.R")

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


