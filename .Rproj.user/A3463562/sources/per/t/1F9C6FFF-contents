library(tidyverse)
library(tidyquant)
library(lubridate)
library(Cairo)
library(showtext)
showtext_auto(enable=T)


tq_transmute_fun_options() %>% str()
tq_transmute_fun_options()$zoo
tq_transmute_fun_options()$xts
tq_transmute_fun_options()$quantmod
tq_transmute_fun_options()$TTR
tq_transmute_fun_options()$PerformanceAnalytics

#data("FANG")
########################FANG化##########################
HU <- getSymbols("2603.tw", auto.assign = FALSE, from = "2017-01-01")
HU <- HU[(rowSums(is.na(HU)) == 0), ]
HU <- tail(HU, n = 100)
HU <- round(HU, digits = 2)
HU <- as.data.frame(HU)
HU <- cbind(date = rownames(HU), HU)
names = gsub("^........(.*$)", "\\1", names(HU))#點數等於字數
names(HU) <- tolower(names)
rownames(HU) <- 1:nrow(HU)
HU <- cbind(symbol = "長榮", HU)
HU$date <- as.Date(HU$date, format =  "%Y-%m-%d")
HU <- as.tibble(HU)

########################################################
CG <- getSymbols("2609.tw", auto.assign = FALSE, from = "2017-01-01")
CG <- CG[(rowSums(is.na(CG)) == 0), ]
CG <- tail(CG, n = 100)
CG <- round(CG, digits = 2)
CG <- as.data.frame(CG)
CG <- cbind(date = rownames(CG), CG)
names = gsub("^........(.*$)", "\\1", names(CG))#點數等於字數
names(CG) <- tolower(names)
rownames(CG) <- 1:nrow(CG)
CG <- cbind(symbol = "陽明", CG)
CG$date <- as.Date(CG$date, format =  "%Y-%m-%d")
CG <- as.tibble(CG)


UD <- getSymbols("2337.tw", auto.assign = FALSE, from = "2017-01-01")
UD <- UD[(rowSums(is.na(UD)) == 0), ]
UD <- tail(UD, n = 100)
UD <- round(UD, digits = 2)
UD <- as.data.frame(UD)
UD <- cbind(date = rownames(UD), UD)
names = gsub("^........(.*$)", "\\1", names(UD))#點數等於字數
names(UD) <- tolower(names)
rownames(UD) <- 1:nrow(UD)
UD <- cbind(symbol = "旺宏", UD)
UD$date <- as.Date(UD$date, format =  "%Y-%m-%d")
UD <- as.tibble(UD)


CC <- getSymbols("2337.tw", auto.assign = FALSE, from = "2017-01-01")
CC <- CC[(rowSums(is.na(CC)) == 0), ]
CC <- tail(CC, n = 100)
CC <- round(CC, digits = 2)
CC <- as.data.frame(CC)
CC <- cbind(date = rownames(CC), CC)
names = gsub("^........(.*$)", "\\1", names(CC))#點數等於字數
names(CC) <- tolower(names)
rownames(CC) <- 1:nrow(CC)
CC <- cbind(symbol = "記憶體-旺宏", CC)
CC$date <- as.Date(CC$date, format =  "%Y-%m-%d")
CC <- as.tibble(CC)

GG <- getSymbols("3532.tw", auto.assign = FALSE, from = "2017-01-01")
GG <- GG[(rowSums(is.na(GG)) == 0), ]
GG <- tail(GG, n = 100)
GG <- round(GG, digits = 2)
GG <- as.data.frame(GG)
GG <- cbind(date = rownames(GG), GG)
names = gsub("^........(.*$)", "\\1", names(GG))#點數等於字數
names(GG) <- tolower(names)
rownames(GG) <- 1:nrow(GG)
GG <- cbind(symbol = "矽晶-台勝科", GG)
GG$date <- as.Date(GG$date, format =  "%Y-%m-%d")
GG <- as.tibble(GG)

YY <- getSymbols("1904.tw", auto.assign = FALSE, from = "2017-01-01")
YY <- YY[(rowSums(is.na(YY)) == 0), ]
YY <- tail(YY, n = 100)
YY <- round(YY, digits = 2)
YY <- as.data.frame(YY)
YY <- cbind(date = rownames(YY), YY)
names = gsub("^........(.*$)", "\\1", names(YY))#點數等於字數
names(YY) <- tolower(names)
rownames(YY) <- 1:nrow(YY)
YY <- cbind(symbol = "紙-正隆", YY)
YY$date <- as.Date(YY$date, format =  "%Y-%m-%d")
YY <- as.tibble(YY)

rr <- getSymbols("1441.tw", auto.assign = FALSE, from = "2017-01-01")
rr <- rr[(rowSums(is.na(rr)) == 0), ]
rr <- tail(rr, n = 100)
rr <- round(rr, digits = 2)
rr <- as.data.frame(rr)
rr <- cbind(date = rownames(rr), rr)
names = gsub("^........(.*$)", "\\1", names(rr))#點數等於字數
names(rr) <- tolower(names)
rownames(rr) <- 1:nrow(rr)
rr <- cbind(symbol = "大東", rr)
rr$date <- as.Date(rr$date, format =  "%Y-%m-%d")
rr <- as.tibble(rr)



FANG <- rbind(HU, CC , CG,rr)
  #HU,CC, CG,UD, GG, YY, rr)



FANG_macd <- FANG %>%
  group_by(symbol) %>%
  tq_mutate(select     = close,
            mutate_fun = MACD,
            nFast      = 12,
            nSlow      = 26,
            nSig       = 9,
            maType     = SMA) %>%
  mutate(diff = macd - signal) %>%
  select(-(open:volume))
FANG_macd

FANG_macd %>%
  filter(date >= as_date("2021-03-01")) %>%
  ggplot(aes(x = date)) +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  geom_line(aes(y = macd, col = symbol)) +
  geom_line(aes(y = signal), color = "blue", linetype = 2) +
  geom_bar(aes(y = diff), stat = "identity", color = palette_light()[[1]]) +
  facet_wrap(~ symbol, ncol = 2, scale = "free_y") +
  labs(title = "Moving Average Convergence Divergence",
       y = "MACD", x = "", color = "") +
  theme_tq() +
  scale_color_tq()

