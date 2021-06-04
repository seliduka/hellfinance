#########################################################
#library("rvest")
#library("magrittr")
#
#url ="http://www.e-fpg.com.tw/j2pt/#02"
#
#htmlcontent = read_html(url)
#
#titlePath = "/html/body/div[1]/div[2]/div[3]/div[6]/table/tbody/tr/td[1]/span/a"
#title = htmlcontent %>% html_nodes(xpath = titlePath) %>% html_text()
#
#replyPath = "/html/body/div[1]/div[3]/div[2]/div[3]/div[6]/table/tbody/tr/td[2]"
#replies = htmlcontent %>% html_nodes(xpath = replyPath) %>% html_text()
#
#myTable = data.frame(Title = title, Reply = replies)
#view(my_table)
#########################################################
# 安裝 jsonlite、rvest 與 magrittr
pkgs <- c("jsonlite", "rvest", "magrittr")
install.packages(pkgs)
# 載入 jsonlite、rvest 與 magrittr
library(jsonlite)
library(rvest)
library(magrittr)
###############
library(jsonlite)

aqi_url <- "http://www.e-fpg.com.tw/j2pt/#02"
aqi <- fromJSON(aqi_url)
class(aqi)
head(aqi)
###############
library(xml2)
library(magrittr)
aqi_url <- "https://opendata.epa.gov.tw/ws/Data/AQI/?$format=xml"
aqi <- read_xml(aqi_url)
class(aqi)
site_names <- aqi %>%
  xml_find_all(xpath = "//Data/SiteName") %>%
  xml_text()
class(site_names)
site_names
###############
library(rvest)
library(magrittr)

movie_url <- "https://www.imdb.com/title/tt4154756"
movie <- read_html(movie_url)
class(movie)
rating <- movie %>%
  html_nodes(css = "strong span") %>%
  html_text() %>%
  as.numeric()
rating

url <- 'https://www.imdb.com/title/tt4154756'
download.file(url)x
##############
install.packages(XML)
library(XML)
url="https://xxxxx.xxxxxx.xxx"
tables1 = readHTMLTable(url)
names(tables1)
tables1[[2]]

readHTMLTable(URL, which =1, header = FALSE, stringsAsFactors = FALSE)
##############









