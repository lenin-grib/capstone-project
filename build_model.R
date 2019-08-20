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

teststr <- "If this isn't"
simplepred4(teststr)
simplepred5(teststr)
wght <- c(0.01,10,1000,100000,1000000)
simplepred4(teststr,wght[1:4])
simplepred5(teststr,wght)

####### test accuracy on validation dataset

names <- c("Ngrams", "samplesize", "weights", "accuracy", "time")
validsumup <- as.data.frame(matrix(ncol = 5, nrow = 6))
colnames(validsumup) <- names

valid_full <- readRDS("merged_valid.RDS")

#### sample 1000 lines from validation set and test few approaches on it
set.seed(20082019)
ns = 1000 
valid <- sample_n(valid_full,ns)

corpus <- cleancorpus(valid)

wght1 <- c(0.01,10,1000,100000,10000000)
wght2 <- c(0.001,1,1000,1000000,100000000)

##### test with ngrams up to 4
ngr4 <- custngram(corpus,4)
quaddf <- makeDF(ngr4)
validdf4 <- separate_last(quaddf,4) %>%
        select(-total)


##### test with ngrams up to 4, no weights
timer <- round(system.time({acc <- validate_pred(validdf4)}),3)
validsumup[1,] <- c(4, ns, "none", acc, timer[1])

##### test with ngrams up to 4, with weights 1

timer <- round(system.time({acc <- validate_pred(validdf4, wght1)}),3)
validsumup[2,] <- c(4, ns, paste(wght1,collapse = " "), acc, timer[1])

##### test with ngrams up to 4, with weights 2

timer <- round(system.time({acc <- validate_pred(validdf4, wght2)}),3)
validsumup[3,] <- c(4, ns, paste(wght2,collapse = " "), acc, timer[1])

##### test with ngrams up to 5
ngr5 <- custngram(corpus,5)
pendf <- makeDF(ngr5)
validdf5 <- separate_last(pendf,5) %>%
        select(-total)

#### test with ngrams up to 5, no weights

timer <- round(system.time({acc <- validate_pred(validdf5)}),3)
validsumup[4,] <- c(5, ns, "none", acc, timer[1])

#### test with ngrams up to 5, with weights 1

timer <- round(system.time({acc <- validate_pred(validdf5, wght1)}),3)
validsumup[5,] <- c(5, ns, paste(wght1,collapse = " "), acc, timer[1])

#### test with ngrams up to 5, with weights 2

timer <- round(system.time({acc <- validate_pred(validdf5, wght2)}),3)
validsumup[6,] <- c(5, ns, paste(wght2,collapse = " "), acc, timer[1])

#### RESULTS
validsumup