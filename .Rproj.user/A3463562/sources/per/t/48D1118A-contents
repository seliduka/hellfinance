getMA <- function(stock){
library(quantmod)
library(xts)
library(zoo)

#rm(list = ls())
YMA <- function(YMA){
  buffer <- tail(YMA,n = 240)
  buffer <- (sum(buffer[,1]))/240
}
HMA <- function(HMA){
  buffer <- tail(HMA,n = 120)
  buffer <- (sum(buffer[,1]))/120
}
SMA <- function(SMA){
  buffer <- tail(SMA,n = 60)
  buffer <- (sum(buffer[,1]))/60
}
MMA <- function(MMA){
  buffer <- tail(MMA,n = 20)
  buffer <- (sum(buffer[,1]))/20
}
TMA <- function(TMA){
  buffer <- tail(TMA,n = 15)
  buffer <- (sum(buffer[,1]))/15
}
FMA <- function(FMA){
  buffer <- tail(FMA,n = 5)
  buffer <- (sum(buffer[,1]))/5
  FMA <- buffer
}


#getSymbols("^TWII", from="2010-1-1") #大盤
#tw2395 = getSymbols('2395.TW', auto.assign = FALSE) #得到研華資料(來源不明)
stock <- getSymbols(stock, auto.assign = FALSE, from="2019-1-1")
#tw=getSymbols('2395.TW', auto.assign = FALSE, from="2019-1-1")
#plot(tw2395) #資料視覺化
#chart_Series(tw2395) #quantmod模組的序列視覺化強化版

head(stock) #查看數據前六行

closing <- stock[,4] #把矩陣tw2395的第四行取出變成closing
closing <- as.matrix(closing)#轉換數列為矩陣
#rownames(closing) <- 1:nrow(closing)#把矩陣排名稱改成1~盡頭
colnames(closing) <- ("y")#把矩陣列名改成y
#plot(closing) #資料視覺化

#p <- data.frame(x=1:nrow(closing), y=closing)#宣告資料表P
#p <- as.matrix(p)#轉換資料表p為矩陣

report <- data.frame(YMA(closing),HMA(closing),SMA(closing),MMA(closing),TMA(closing),FMA(closing))
colnames(report) <- c('YMA', 'HMA', 'SMA', 'MMA', 'TMA','FMA')
#row.names(report) <- (stock)
return(report)
}
