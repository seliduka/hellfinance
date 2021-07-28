library(tidyverse)
library(tidyquant)
library(lubridate)
library(Cairo)
library(showtext)
library(readxl)
library(xlsx)
showtext_auto(enable=T)

thexml <- read.xlsx("more.xlsx",sheetName = "Sheet1" )
thexml$NA. <- NULL
thexml$date <- as.Date(thexml$date, format =  "%Y-%m-%d")
#thexml <- thexml[-c(14),]
dday <- nrow(thexml)
thexml$numb <- as.numeric(thexml$numb)
ggplot(thexml, aes(x=date)) + geom_line(aes(y = numb), color = "darkred")
plot(thexml)


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
thexml$move <- ((HU$close-HU$low))*100
#thexml$close <- (HU$close-16500)*50
thexml$don <- ((HU$high-HU$low)-200)*100
thexml$up <- ((HU$close-HU$open)-200)*100
thexml$go <- (HU$open-16500)*50

cor.test(thexml$numb,thexml$move)

ggplot(thexml, aes(x=date)) +
  geom_line(aes(y = numb), color = "darkred", group = 1) +
 # geom_line(aes(x = date, y = GG), color = "green", group = 1) +
 # geom_line(aes(x = date, y = don), color = "chocolate2", group = 1) +
 # geom_line(aes(x = date, y = go), color = "cyan1", group = 1) +
#  geom_line(aes(x = date, y = up), color = "plum2", group = 1)
  geom_line(aes(x = date-2, y = move), color = "blue", group = 1) +
  ggtitle("foreign investment LS & close-low \n relationship lines")

#HU$move <- (HU$close-HU$low)*10
#ggplot(HU, aes(x=date)) +
#  geom_line(aes(y = move), color = "blue")



