source("methods.R")

######### optimizing frequency tables

freq1 <- readRDS(file = "freq1.RDS")
freq2 <- readRDS(file = "freq2.RDS")
freq3 <- readRDS(file = "freq3.RDS")
freq4 <- readRDS(file = "freq4.RDS")

freq1sw <- readRDS(file = "freq1stop_removed.RDS")
freqstart <- readRDS(file = "freqstart.RDS")

freq1 <- freq1 %>%
        filter(total >1) %>%
        head(10)
freq2 <- freq2 %>%
        filter(total >1) %>%
        separate_last(2)
freq3 <- freq3 %>%
        filter(total >1) %>%
        separate_last(3)
freq4 <- freq4 %>%
        filter(total >1) %>%
        separate_last(4)

freq1sw <- freq1sw %>%
        filter(total >1) %>%
        head(10)

teststr <- "stop Being So"
simplepred(teststr)



