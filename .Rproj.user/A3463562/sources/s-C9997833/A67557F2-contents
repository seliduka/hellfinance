getTWVAL <- function(sto, dayss){
  library(quantmod)
  library(xts)
  library(zoo)
  library(lubridate)
  library(ggplot2)
  library(babynames)
  library(dplyr)

  #rm(list = ls())

  AAPL = getSymbols(sto, auto.assign = FALSE)
  buffer <- data.frame(AAPL)
  buffer <- buffer[(rowSums(is.na(buffer)) == 0), ]
  names = gsub("^.....(.*$)", "\\1", names(AAPL))#點數等於字數
  names(buffer) <- names
  buffer$tag <- ifelse(buffer$Open > buffer$Close, buffer$tag <- "down", buffer$tag <- "up")
  buffer$Volume <- ifelse(buffer$tag == "down", buffer$Volume*(-1), buffer$Volume)
  geow <- data.frame("index" = buffer$Volume)
  geow$dates <- rownames(buffer)
  geow$colour <- buffer$tag
  bu2 <- data.frame(geow)
  geow <- tail(bu2,n = dayss)
  ggplot(geow,aes(dates,index,label=""))+
    geom_bar(stat="identity",position="identity",aes(fill = colour))+
    scale_fill_manual(name = "box", values=c(down="firebrick",up="dodgerblue4"))+
    ggtitle("Index")+
    theme(plot.title = element_text(hjust = 0.5))+
    xlab("days") + ylab("amount")
}

getTWVAL("^TWII", 60)
###################another###################
#buffer2 <- data.frame(AAPL)
#buffer2 <- buffer[(rowSums(is.na(buffer2)) == 0), ]
#names(buffer2) <- names
#buffer2$tag <- ifelse(buffer2$High > buffer2$Close, buffer2$tag <- "M", buffer2$tag <- "U")
#buffer2$Volume <- ifelse(buffer2$tag == "M", buffer2$Volume*(-1), buffer2$Volume)
#geow2 <- data.frame("index" = buffer2$Volume)
#geow2$dates <- rownames(buffer2)
#geow2$colour <- buffer2$tag
#bu4 <- data.frame(geow2)
#geow2 <- tail(bu4,n = 100)
#ggplot(geow2,aes(dates,index,label=""))+
#  geom_bar(stat="identity",position="identity",aes(fill = colour))+
#  scale_fill_manual(name = "box", values=c(M="firebrick",U="dodgerblue4"))+
#  ggtitle("Index")+
# theme(plot.title = element_text(hjust = 0.5))+
#xlab("Year") + ylab("S.D.")





