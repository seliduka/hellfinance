library(tidyverse)
library(tidyquant)
library(lubridate)

HU <- getSymbols("^TWII", auto.assign = FALSE, from = "2017-01-01")
HU <- HU[(rowSums(is.na(HU)) == 0), ]
HU <- tail(HU, n = 356)
HU <- round(HU, digits = 2)
HU <- as.data.frame(HU)
HU <- cbind(date = rownames(HU), HU)
names = gsub("^.....(.*$)", "\\1", names(HU))#點數等於字數
names(HU) <- tolower(names)
rownames(HU) <- 1:nrow(HU)
HU <- cbind(symbol = "GG", HU)
HU$date <- as.Date(HU$date, format =  "%Y-%m-%d")
HU <- as.tibble(HU)

FANG_macd <- HU %>%
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
  filter(date >= as_date("2020-01-01")) %>%
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

