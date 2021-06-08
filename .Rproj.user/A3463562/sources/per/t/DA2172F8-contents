MAmovement <- function(st, daysss){
library(quantmod)
library(xts)
library(zoo)
library(lubridate)
library(ggplot2)
library(babynames)
library(dplyr)
##^TWII
#rm(list = ls())

dao <- st
  allline <- getSymbols(dao, auto.assign = FALSE, from = "2013-01-01")
  #必須先去除NA值
  YMAline <- allline[(rowSums(is.na(allline)) == 0), ]
  YMAline <- data.frame(allline[, 4])
  does <- data.frame()
  for (c in 244:(nrow(YMAline))) {
    so <- sum(YMAline[c:(c-244), 1]) / 245

    so <- data.frame(rbind(does, so))
    does <- so
  }
  colnames(so) <- "YMA"
  YMAline <- data.frame(YMAline)
  rownames(so) <- tail(rownames(YMAline), nrow(so))
  so <- round(so, digits = 2)
#  chartSeries(so)
  YMAline <- so

  HMAline <- allline[(rowSums(is.na(allline)) == 0), ]
  HMAline <- data.frame(allline[, 4])
  does1 <- data.frame()
  for (c in 122:(nrow(HMAline))) {
    so1 <- sum(HMAline[c:(c-122), 1]) / 123

    so1 <- data.frame(rbind(does1, so1))
    does1 <- so1
  }
  colnames(so1) <- "HMA"
  HMAline <- data.frame(HMAline)
  rownames(so1) <- tail(rownames(HMAline), nrow(so1))
  so1 <- round(so1, digits = 2)
#  chartSeries(so)
  HMAline <- so1

  SMAline <- allline[(rowSums(is.na(allline)) == 0), ]
  SMAline <- data.frame(allline[, 4])
  does6 <- data.frame()
  for (c in 60:(nrow(SMAline))) {
    so6 <- sum(SMAline[c:(c-60), 1]) / 61

    so6 <- data.frame(rbind(does6, so6))
    does6 <- so6
  }
  colnames(so6) <- "SMA"
  SMAline <- data.frame(SMAline)
  rownames(so6) <- tail(rownames(SMAline), nrow(so6))
  so6 <- round(so6, digits = 2)
  #  chartSeries(so)
  SMAline <- so6

  MMAline <- allline[(rowSums(is.na(allline)) == 0), ]
  MMAline <- data.frame(allline[, 4])
  does2 <- data.frame()
  for (c in 20:(nrow(MMAline))) {
    so2 <- sum(MMAline[c:(c-20), 1]) / 21

    so2 <- data.frame(rbind(does2, so2))
    does2 <- so2
  }
  colnames(so2) <- "MMA"
  MMAline <- data.frame(MMAline)
  rownames(so2) <- tail(rownames(MMAline), nrow(so2))
  so2 <- round(so2, digits = 2)
 # chartSeries(so)
  MMAline <- so2

  TMAline <- allline[(rowSums(is.na(allline)) == 0), ]
  TMAline <- data.frame(allline[, 4])
  does3 <- data.frame()
  for (c in 15:(nrow(TMAline))) {
    so3 <- sum(TMAline[c:(c-15), 1]) / 16

    so3 <- data.frame(rbind(does3, so3))
    does3 <- so3
  }
  colnames(so3) <- "TMA"
  TMAline <- data.frame(TMAline)
  rownames(so3) <- tail(rownames(TMAline), nrow(so3))
  so3 <- round(so3, digits = 2)
#  chartSeries(so)
  TMAline <- so3

  FMAline <- allline[(rowSums(is.na(allline)) == 0), ]
  FMAline <- data.frame(allline[, 4])
  does4 <- data.frame()
  for (c in 5:(nrow(FMAline))) {
    so4 <- sum(FMAline[c:(c-5), 1]) / 6

    so4 <- data.frame(rbind(does4, so4))
    does4 <- so4
  }
  colnames(so4) <- "FMA"
  FMAline <- data.frame(FMAline)
  rownames(so4) <- tail(rownames(FMAline), nrow(so4))
  so4 <- round(so4, digits = 2)
#  chartSeries(so)
  FMAline <- so4

#chart_Series(Yline(),)
#add_TA(Hline(), on = 1, col = "blue", lty =2)
#plot.xts(merge(VXX[,6],VIX[,6],join = 'inner'),main = 'plot 2 series on same panel')

ALL <- data.frame()
#ALL <- as.data.frame(cbind("Yline" = Yline(a)[,1],"Hline" = Hline(a)[,1],"Mline" = Mline(a)[,1],"Tline" = Tline(a)[,1],"Fline" = Fline(a)[,1]))

#rownames(ALL)<- rownames(FMAline)
#plot(ALL)
ldate <- nrow(YMAline)
YMAline <- tail(YMAline, n=ldate)
HMAline <- tail(HMAline, n=ldate)
SMAline <- tail(SMAline, n=ldate)
MMAline <- tail(MMAline, n=ldate)
TMAline <- tail(TMAline, n=ldate)
FMAline <- tail(FMAline, n=ldate)

ALL <- as.data.frame(cbind(x = (1:nrow(YMAline)), YMA = YMAline$YMA,HMA = HMAline$HMA,SMA = SMAline$SMA, MMA = MMAline$MMA, TMA = TMAline$TMA, FMA = FMAline$FMA))
#YMAline <- cbind(YMAline,x = (1:nrow(YMAline)))
#ggplot(ALL, aes(x = x, y = YMA)) + geom_line()
bu1 <- ALL
ALL <- tail(bu1, n = daysss)

ggplot(ALL, aes(x=x)) +
  geom_line(aes(y = YMA), color = "darkred")+
  geom_line(aes(y = HMA), color="orange")+
  geom_line(aes(y = SMA), color="yellow")+
  geom_line(aes(y = MMA), color="green")+
  geom_line(aes(y = TMA), color="blue")+
  geom_line(aes(y = FMA), color="slateblue")
}

###############
getMA("2603.tw")

getTWVAL("^TWII" , 10)
MAmovement("2603.tw" , 60)
