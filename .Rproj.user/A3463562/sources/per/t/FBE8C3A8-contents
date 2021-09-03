library(tidyverse)
library(tidyquant)

HU <- getSymbols("2603.tw", auto.assign = FALSE, from = "2017-01-01")
HU <- HU[(rowSums(is.na(HU)) == 0), ]
HU <- tail(HU, n = 100)
HU <- round(HU, digits = 2)
HU <- as.data.frame(HU)
HU <- cbind(date = rownames(HU), HU)
names = gsub("^........(.*$)", "\\1", names(HU))#點數等於字數
names(HU) <- tolower(names)
rownames(HU) <- 1:nrow(HU)
HU <- cbind(symbol = "GG", HU)
HU$date <- as.Date(HU$date, format =  "%Y-%m-%d")
HU <- as.tibble(HU)

HU %>%
  ggplot(aes(x = date, y = close)) +
  geom_barchart(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "AAPL Bar Chart", y = "Closing Price", x = "") +
  theme_tq()
