source("methods.R")

######### optimizing frequency tables

freq1 <- readRDS(file = "freq1.RDS")
freq2 <- readRDS(file = "freq2.RDS")
freq3 <- readRDS(file = "freq3.RDS")
freq4 <- readRDS(file = "freq4.RDS")
freq5 <- readRDS(file = "freq5.RDS")

freq1sw <- readRDS(file = "freq1stopword_removed.RDS")
freqstart <- readRDS(file = "freqstart.RDS")

freq1 <- preparefreq(freq1,1)
freqstart <- preparefreq(freqstart,1)
freq1sw <- preparefreq(freq1sw,1)

freq2 <- preparefreq(freq2,2)
freq3 <- preparefreq(freq3,3)
freq4 <- preparefreq(freq4,4)
freq5 <- preparefreq(freq5,5)

teststr <- "blah blah"
simplepred4(teststr)
simplepred5(teststr)
wght <- c(0.01,10,10000,1000000,1000000)
simplepred4(teststr,wght[1:4])
simplepred5(teststr,wght)

####### test accuracy on validation dataset

names <- c("Ngrams", "samplesize", "weights", "accuracy")
validsumup <- as.data.frame(matrix(ncol = 4, nrow = 4))
colnames(validsumup) <- names

valid_full <- readRDS("merged_valid.RDS")

set.seed(13)
ns = 1000
valid <- sample_n(valid_full,ns)

corpus <- cleancorpus(valid)

names <- c("term", "real", "prediction", "hit")
validres <- as.data.frame(matrix(ncol = 4, nrow = nrow(validdf)))
colnames(validres) <- names

##### test with 4grams
ngr4 <- custngram(corpus,4)
quaddf <- makeDF(ngr4)
validdf <- separate_last(quaddf,4) %>%
        select(-total)

##### no weights
for (i in 1:nrow(validdf)){
     validres[i,1] <- validdf[i,1]
     validres[i,2] <- validdf[i,2]
     validres[i,3] <- paste(simplepred4(validdf[i,1]), 
             collapse=" ")
     validres[i,4] <- grepl(validres[i,2], validres[i,3], fixed=TRUE)
}

acc1 <- paste0(round(nrow(validres[validres$hit == TRUE,])/
                nrow(validres)*100, 2),"%")

validsumup[1,] <- c(4, ns, "none", acc1)

##### test with 4grams, with weights

wght <- c(0.1,10,10000,10000000)

for (i in 1:nrow(validdf)){
        validres[i,1] <- validdf[i,1]
        validres[i,2] <- validdf[i,2]
        validres[i,3] <- paste(simplepred4(validdf[i,1],wght), 
                collapse=" ")
        validres[i,4] <- grepl(validres[i,2], validres[i,3], fixed=TRUE)
}

acc2 <- paste0(round(nrow(validres[validres$hit == TRUE,])/
                nrow(validres)*100, 2),"%")

validsumup[2,] <- c(4, ns, paste(wght,collapse = " "), acc2)



##### test with 5grams
ngr5 <- custngram(corpus,5)
pendf <- makeDF(ngr5)
validdf <- separate_last(quaddf,5) %>%
        select(-total)

#### no weights

for (i in 1:nrow(validdf)){
        validres[i,1] <- validdf[i,1]
        validres[i,2] <- validdf[i,2]
        validres[i,3] <- paste(simplepred5(validdf[i,1]), 
                collapse=" ")
        validres[i,4] <- grepl(validres[i,2], validres[i,3], fixed=TRUE)
}

acc3 <- paste0(round(nrow(validres[validres$hit == TRUE,])/
                nrow(validres)*100, 2),"%")
validsumup[3,] <- c(5, ns, "none", acc3)

#### with weights
wght <- c(0.1,10,10000,10000000, 10000000000)

for (i in 1:nrow(validdf)){
        validres[i,1] <- validdf[i,1]
        validres[i,2] <- validdf[i,2]
        validres[i,3] <- paste(simplepred5(validdf[i,1], wght), 
                collapse=" ")
        validres[i,4] <- grepl(validres[i,2], validres[i,3], fixed=TRUE)
}

acc4 <- paste0(round(nrow(validres[validres$hit == TRUE,])/
                nrow(validres)*100, 2),"%")
validsumup[3,] <- c(5, ns, paste(wght,collapse = " "), acc4)