library(quantmod)
library(xts)
library(zoo)
library(lubridate)

rm(list = ls())

somename <- data.frame("3049.tw", "2108.tw")#輸入資訊

get_all_MA <- function(stockname){
buffer <- stockname
oneyearago <- today()-365#去年的今天
Nullone <- data.frame()#在下列for時使用
for (c in 1:ncol(buffer)){
sname <- buffer[1,c]
sanp <- getSymbols(toString(sname), auto.assign = FALSE, from=toString(oneyearago))
MA <- getMA(sname)
rownames(MA) <- toString(sname)
MAlist <- rbind(Nullone,assign(sname, MA))
Nullone <- MA#必須將資料暫存，要不然無法rbind
}
return(MAlist)
}

get_all_MA(somename)
#buffer <- getSymbols(toString(buffer), auto.assign = FALSE, from=toString(oneyearago))
#toString(oneyearago)
#toString(buffer)
#buffer[1,1]:buffer[nrow(buffer),1]
#getSymbols("3049.tw", auto.assign = FALSE, from=toString(oneyearago))
#buffer <- data.frame(paste(1:9999, '.tw', sep = ""))
#buffer[1:nrow(buffer),1]
#buffer <- buffer[3049,1]

#skip_to_next <- FALSE
#tryCatch(print(b), error = function(e) { skip_to_next <<- TRUE})
#if(skip_to_next) { next }
