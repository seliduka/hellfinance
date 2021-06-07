library(tidyverse)
library(tidyquant)
library(lubridate)
library(Cairo)
library(showtext)
showtext_auto(enable=T)

thexml <- read.xlsx("more.xlsx",sheetName = "Sheet1" )
thexml$NA. <- NULL
thexml$date <- as.Date(thexml$date, format =  "%Y-%m-%d")
ggplot(thexml, aes(x=date)) + geom_line(aes(y = numb), color = "darkred")

dday <- nrow(thexml)

HU <- getSymbols("^TWII", auto.assign = FALSE, from = "2017-01-01")
HU <- HU[(rowSums(is.na(HU)) == 0), ]
HU <- tail(HU, n = dday)
HU <- round(HU, digits = 2)
HU <- as.data.frame(HU)
HU <- cbind(date = rownames(HU), HU)
names = gsub("^.....(.*$)", "\\1", names(HU))#點數等於字數
names(HU) <- tolower(names)
rownames(HU) <- 1:nrow(HU)
HU <- cbind(symbol = "GG", HU)
HU$date <- as.Date(HU$date, format =  "%Y-%m-%d")
HU <- as.tibble(HU)
thexml <- cbind(thexml, HU$close)
thexml$`HU$close`<- thexml$`HU$close`-16000
ggplot(thexml, aes(x=date)) + geom_line(aes(y = numb), color = "darkred") + geom_line(aes(y = thexml$`HU$close`), color = "blue")

