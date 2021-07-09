library(tidyverse)
library(tidyquant)
library(lubridate)
library(Cairo)
library(showtext)
showtext_auto(enable=T)

HU <- getSymbols("^TWII", auto.assign = FALSE, from = "2017-01-01")
HU <- HU[(rowSums(is.na(HU)) == 0), ]
HU <- tail(HU, n = 100)
HU <- round(HU, digits = 2)
HU <- as.data.frame(HU)
HU <- cbind(date = rownames(HU), HU)
names = gsub("^.....(.*$)", "\\1", names(HU))#點數等於字數
names(HU) <- tolower(names)
rownames(HU) <- 1:nrow(HU)
HU <- cbind(symbol = "GG", HU)
HU$date <- as.Date(HU$date, format =  "%Y-%m-%d")
HU <- as.tibble(HU)

url <- "https://www.taifex.com.tw/cht/3/futContractsDate"
html <- read_html(url, encoding = "UTF-8")

html_name(html)
html_attrs(html)
html_attr(html, "div")

cast <- html_nodes(html, "td:nth-child(12) font , td:nth-child(14) div")
cast <- gsub("[^-0-9+]", "\\1", cast[5])

cast <- data.frame(date = today(), numb = cast)
#write.xlsx(cast, file = "more.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
#df <- read.xlsx("more.xlsx",sheetName = "Sheet1" )
#df$NA. <- NULL
#write.xlsx(rbind(df, cast), file = "more.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

