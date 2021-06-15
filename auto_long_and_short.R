library(rvest) # 爬蟲相關套件
library(httr) # 爬蟲相關套件
library(dplyr) # 非常強大的資料處理套件
library(lubridate) # 處理日期變數的套件
library(stringr) # 處理字串的套件
library(xlsx)

getOption('timeout')
options(timeout=100)

url <- "https://www.taifex.com.tw/cht/3/totalTableDate"
html <- read_html(url, encoding = "UTF-8")

html_name(html)
html_attrs(html)
html_attr(html, "div")

cast <- html_nodes(html, "td:nth-child(7) b")
cast <- gsub("[^-0-9+]", "\\1", cast[1])
cast <- data.frame(date = today(), numb = cast)
#write.xlsx(cast, file = "more.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
df <- read.xlsx("more.xlsx",sheetName = "Sheet1" )
df$NA. <- NULL
write.xlsx(rbind(df, cast), file = "more.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
#每上班日下午14:10分自動執行

