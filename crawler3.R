#install.packages("rvest")
library(rvest)
html <- read_html("http://www.imdb.com/title/tt1490017/")
cast <- html_nodes(html, "span.itemprop")
html_text(cast)
cast <- html_nodes(html, "#titleCast .itemprop")
html_text(cast)
cast <- html_nodes(html, "#titleCast span.itemprop")
html_text(cast)

library(tmcn)
html <- read_html("https://news.ltn.com.tw/")
news.title <- html_nodes(html, ".picword")
news.title.utf8 <- toUTF8(html_text(news.title)) # convert to UTF8
title.href <- html_attr(news.title, "href")
my.news <- data.frame(title = news.title.utf8, href = title.href, stringsAsFactors=FALSE)
my.news

appledaily <- "http://www.appledaily.com.tw"
url.main <- paste0(appledaily, "/realtimenews/section/new/")
apple.news <- read_html(url.main)
 # news.rddt <- html_nodes(apple.news, '.rtddt')
 # news.time <- html_text(html_nodes(news.rddt, 'time'))

news.time <- html_text(html_nodes(apple.news, '.rtddt time'))
news.title <- html_text(html_nodes(apple.news, '.rtddt h1'))
news.category <- html_text(html_nodes(apple.news, '.rtddt h2'))
news.url <- html_attr(html_nodes(apple.news, '.rtddt a'), 'href')
realtimenews <- data.frame(time=news.time, title=news.title,
                           + category=news.category, url= paste0(appledaily, news.url))
realtimenews


url.main <- 'https://www.ptt.cc/bbs/R_Language/index.html'
href.title <- html_nodes(read_html(url.main), ".title a")
R.hrefs <- html_attr(href.title, 'href')

R.article.data <- c()
for(i in 1:length(R.hrefs)){
  article.url <- paste0('https://www.ptt.cc', R.hrefs[i])
  article <- html_nodes(read_html(article.url), "#main-content")
  article.content <- html_text(article)
  article.utf8 <- iconv(article.content, 'utf8')
  R.article.data <- c(R.article.data, article.utf8)
  Sys.sleep(sample(3:5, 1))
  }
R.article.data
