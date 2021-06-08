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
HU <- getSymbols("2002.tw", auto.assign = FALSE, from = "2017-01-01")
HU <- HU[(rowSums(is.na(HU)) == 0), ]
HU <- tail(HU, n = 100)
HU <- round(HU, digits = 2)
HU <- as.data.frame(HU)
HU <- cbind(date = rownames(HU), HU)
names = gsub("^........(.*$)", "\\1", names(HU))#點數等於字數
names(HU) <- tolower(names)
rownames(HU) <- 1:nrow(HU)
HU <- cbind(symbol = "中鋼", HU)
HU$date <- as.Date(HU$date, format =  "%Y-%m-%d")
HU <- as.tibble(HU)

########################################################
CG <- getSymbols("2009.tw", auto.assign = FALSE, from = "2017-01-01")
CG <- CG[(rowSums(is.na(CG)) == 0), ]
CG <- tail(CG, n = 100)
CG <- round(CG, digits = 2)
CG <- as.data.frame(CG)
CG <- cbind(date = rownames(CG), CG)
names = gsub("^........(.*$)", "\\1", names(CG))#點數等於字數
names(CG) <- tolower(names)
rownames(CG) <- 1:nrow(CG)
CG <- cbind(symbol = "第一銅", CG)
CG$date <- as.Date(CG$date, format =  "%Y-%m-%d")
CG <- as.tibble(CG)


UD <- getSymbols("2317.tw", auto.assign = FALSE, from = "2017-01-01")
UD <- UD[(rowSums(is.na(UD)) == 0), ]
UD <- tail(UD, n = 100)
UD <- round(UD, digits = 2)
UD <- as.data.frame(UD)
UD <- cbind(date = rownames(UD), UD)
names = gsub("^........(.*$)", "\\1", names(UD))#點數等於字數
names(UD) <- tolower(names)
rownames(UD) <- 1:nrow(UD)
UD <- cbind(symbol = "鴻海", UD)
UD$date <- as.Date(UD$date, format =  "%Y-%m-%d")
UD <- as.tibble(UD)


CC <- getSymbols("6116.tw", auto.assign = FALSE, from = "2017-01-01")
CC <- CC[(rowSums(is.na(CC)) == 0), ]
CC <- tail(CC, n = 100)
CC <- round(CC, digits = 2)
CC <- as.data.frame(CC)
CC <- cbind(date = rownames(CC), CC)
names = gsub("^........(.*$)", "\\1", names(CC))#點數等於字數
names(CC) <- tolower(names)
rownames(CC) <- 1:nrow(CC)
CC <- cbind(symbol = "彩晶", CC)
CC$date <- as.Date(CC$date, format =  "%Y-%m-%d")
CC <- as.tibble(CC)

GG <- getSymbols("2603.tw", auto.assign = FALSE, from = "2017-01-01")
GG <- GG[(rowSums(is.na(GG)) == 0), ]
GG <- tail(GG, n = 100)
GG <- round(GG, digits = 2)
GG <- as.data.frame(GG)
GG <- cbind(date = rownames(GG), GG)
names = gsub("^........(.*$)", "\\1", names(GG))#點數等於字數
names(GG) <- tolower(names)
rownames(GG) <- 1:nrow(GG)
GG <- cbind(symbol = "長榮", GG)
GG$date <- as.Date(GG$date, format =  "%Y-%m-%d")
GG <- as.tibble(GG)

YY <- getSymbols("2609.tw", auto.assign = FALSE, from = "2017-01-01")
YY <- YY[(rowSums(is.na(YY)) == 0), ]
YY <- tail(YY, n = 100)
YY <- round(YY, digits = 2)
YY <- as.data.frame(YY)
YY <- cbind(date = rownames(YY), YY)
names = gsub("^........(.*$)", "\\1", names(YY))#點數等於字數
names(YY) <- tolower(names)
rownames(YY) <- 1:nrow(YY)
YY <- cbind(symbol = "陽明", YY)
YY$date <- as.Date(YY$date, format =  "%Y-%m-%d")
YY <- as.tibble(YY)

FANG <- rbind(CC, CG, HU, UD, GG, YY)



FANG_annual_returns <- FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "yearly",
               type       = "arithmetic")
FANG_annual_returns#年收益

FANG_annual_returns %>%
  ggplot(aes(x = date, y = yearly.returns, fill = symbol)) +
  geom_col() +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "FANG: Annual Returns",
       subtitle = "Get annual returns quickly with tq_transmute!",
       y = "Annual Returns", x = "") +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  theme_tq() +
  scale_fill_tq()#年收益圖表

FANG_daily_log_returns <- FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               type       = "log",
               col_rename = "monthly.returns")

FANG_daily_log_returns %>%
  ggplot(aes(x = monthly.returns, fill = symbol)) +
  geom_density(alpha = 0.5) +
  labs(title = "FANG: Charting the Daily Log Returns",
       x = "Monthly Returns", y = "Density") +
  theme_tq() +
  scale_fill_tq() +
  facet_wrap(~ symbol, ncol = 2)
#每日日誌回報

FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = open:volume,
               mutate_fun = to.period,
               period     = "months")
#改成每月回報

FANG_daily <- FANG %>%
  group_by(symbol)

FANG_daily %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "Daily Stock Prices",
       x = "", y = "Adjusted Prices", color = "") +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() +
  scale_color_tq()
#無週期性聚合

FANG_monthly <- FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = to.period,
               period     = "months")

FANG_monthly %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "Monthly Stock Prices",
       x = "", y = "Adjusted Prices", color = "") +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() +
  scale_color_tq()
#每月定期聚合

# Asset Returns
FANG_returns_monthly <- FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly")

# Baseline Returns
baseline_returns_monthly <- "XLK" %>%
  tq_get(get  = "stock.prices",
         from = "2013-01-01",
         to   = "2016-12-31") %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly")

returns_joined <- left_join(FANG_returns_monthly,
                            baseline_returns_monthly,
                            by = "date")
returns_joined

FANG_rolling_corr <- returns_joined %>%
  tq_transmute_xy(x          = monthly.returns.x,
                  y          = monthly.returns.y,
                  mutate_fun = runCor,
                  n          = 6,
                  col_rename = "rolling.corr.6")

FANG_rolling_corr %>%
  ggplot(aes(x = date, y = rolling.corr.6, color = symbol)) +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  geom_line(size = 1) +
  labs(title = "FANG: Six Month Rolling Correlation to XLK",
       x = "", y = "Correlation", color = "") +
  facet_wrap(~ symbol, ncol = 2) +
  theme_tq() +
  scale_color_tq()
#可視化收益的滾動相關性


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
  filter(date >= as_date("2016-10-01")) %>%
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

ggsave("ggplot2.pdf",device=cairo_pdf,width=8,height=8)
#可視化移動平均收斂散度

FANG_max_by_qtr <- FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = apply.quarterly,
               FUN        = max,
               col_rename = "max.close") %>%
  mutate(year.qtr = paste0(year(date), "-Q", quarter(date))) %>%
  select(-date)
FANG_max_by_qtr

FANG_min_by_qtr <- FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = apply.quarterly,
               FUN        = min,
               col_rename = "min.close") %>%
  mutate(year.qtr = paste0(year(date), "-Q", quarter(date))) %>%
  select(-date)

FANG_by_qtr <- left_join(FANG_max_by_qtr, FANG_min_by_qtr,
                         by = c("symbol"   = "symbol",
                                "year.qtr" = "year.qtr"))
FANG_by_qtr

FANG_by_qtr %>%
  ggplot(aes(x = year.qtr, color = symbol)) +
  geom_segment(aes(xend = year.qtr, y = min.close, yend = max.close),
               size = 1) +
  geom_point(aes(y = max.close), size = 2) +
  geom_point(aes(y = min.close), size = 2) +
  facet_wrap(~ symbol, ncol = 2, scale = "free_y") +
  labs(title = "FANG: Min/Max Price By Quarter",
       y = "Stock Price", color = "") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank())
#獲取每個季度的最高和最低價格


stock_prices <- c("MA", "V") %>%
  tq_get(get  = "stock.prices",
         from = "2015-01-01",
         to   = "2016-12-31") %>%
  group_by(symbol)

stock_pairs <- stock_prices %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               type       = "log",
               col_rename = "returns") %>%
  spread(key = symbol, value = returns)

stock_pairs %>%
  ggplot(aes(x = V, y = MA)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Visualizing Returns Relationship of Stock Pairs") +
  theme_tq()

lm(MA ~ V, data = stock_pairs) %>%
  summary()

regr_fun <- function(data) {
  coef(lm(MA ~ V, data = timetk::tk_tbl(data, silent = TRUE)))
}

stock_pairs <- stock_pairs %>%
  tq_mutate(mutate_fun = rollapply,
            width      = 90,
            FUN        = regr_fun,
            by.column  = FALSE,
            col_rename = c("coef.0", "coef.1"))
stock_pairs

stock_pairs %>%
  ggplot(aes(x = date, y = coef.1)) +
  geom_line(size = 1, color = palette_light()[[1]]) +
  geom_hline(yintercept = 0.8134, size = 1, color = palette_light()[[2]]) +
  labs(title = "MA ~ V: Visualizing Rolling Regression Coefficient", x = "") +
  theme_tq()

stock_prices %>%
  tq_transmute(adjusted,
               periodReturn,
               period = "daily",
               type = "log",
               col_rename = "returns") %>%
  mutate(wealth.index = 100 * cumprod(1 + returns)) %>%
  ggplot(aes(x = date, y = wealth.index, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "MA and V: Stock Prices") +
  theme_tq() +
  scale_color_tq()
#可視化滾動回歸


FANG %>%
  group_by(symbol) %>%
  tq_transmute(adjusted, periodReturn, period = "daily") %>%
  tq_transmute(daily.returns, Return.clean, alpha = 0.05) %>%
  tq_transmute(daily.returns, Return.excess, Rf = 0.03 / 252)

#清理和計算超額收益





















