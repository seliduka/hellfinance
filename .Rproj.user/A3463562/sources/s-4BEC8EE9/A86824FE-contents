getOption('timeout')
options(timeout=100)

COMpanel <- function(days){
library(quantmod)
getSymbols("2409.tw")
#getSymbols("2436.tw") 成長過高
#getSymbols("2458.tw") 成長過高
#getSymbols("3049.tw") 低成長
getSymbols("3481.tw")
getSymbols("3607.tw")
#getSymbols("3622.tw") 大量級
#getSymbols("5471.tw") 成長過高
getSymbols("6116.tw")
getSymbols("6243.tw")


sb = data.frame(`2409.TW`)
#sc = data.frame(`2436.TW`)
#sd = data.frame(`2458.TW`)
#sh = data.frame(`3049.TW`)
sl = data.frame(`3481.TW`)
#sr = data.frame(`3622.TW`)
#sv = data.frame(`5471.TW`)
sy = data.frame(`6116.TW`)

names = gsub("^........(.*$)", "\\1", names(`2409.TW`))
names(sb) = names
#names(sc) = names
#names(sd) = names
#names(sh) = names
names(sl) = names
#names(sr) = names
#names(sv) = names
names(sy) = names


# rbind into one dataframe
sb$label = "友達"
#sc$label = "sc"
#sd$label = "sd"
#sh$label = "和鑫"
sl$label = "群創"
#sr$label = "洋華"
#sv$label = "sv"
sy$label = "彩晶"

df = rbind(tail(sb, n = days), tail(sl, n = days), tail(sy, n = days))

#df = tail(df, n = days)
# Packages
library(ggplot2)
library(directlabels)

# The plot - labels at the beginning and the ends of the lines.
#ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
#  geom_line()  +
#  scale_colour_discrete(guide = 'none')  +
#  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
geom_line()  +
scale_colour_discrete(guide = 'none')  +
geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
}
##########################

COMicdesign <- function(days){
  library(quantmod)
  getSymbols(icdesign)

  sa<- icdesign[1]
  sb<- icdesign[2]
  sc<- icdesign[3]
  sd<- icdesign[4]
  se<- icdesign[5]
#  sf<- icdesign[6]
  sg<- icdesign[7]
  sh<- icdesign[8]
  si<- icdesign[9]
  sj<- icdesign[10]
  sk<- icdesign[11]
  sl<- icdesign[12]


  sa = data.frame(get(sa))
  sb = data.frame(get(sb))
  sc = data.frame(get(sc))
  sd = data.frame(get(sd))
  se = data.frame(get(se))
#  sf = data.frame(get(sf))
  sg = data.frame(get(sg))
  sh = data.frame(get(sh))
  si = data.frame(get(si))
  sj = data.frame(get(sj))
  sk = data.frame(get(sk))
  sl = data.frame(get(sl))

  names = gsub("^........(.*$)", "\\1", names(`2363.TW`))
  names(sa) = names
  names(sb) = names
  names(sc) = names
  names(sd) = names
  names(se) = names
 # names(sf) = names
  names(sg) = names
  names(sh) = names
  names(si) = names
  names(sj) = names
  names(sk) = names
  names(sl) = names

  # rbind into one dataframe
  sa$label = "sa"
  sb$label = "sb"
  sc$label = "sc"
  sd$label = "sd"
  se$label = "se"
 # sf$label = "sf"
  sg$label = "sg"
  sh$label = "sh"
  si$label = "si"
  sj$label = "sj"
  sk$label = "sk"
  sl$label = "sl"

  df = rbind(tail(sa, n = days), tail(sb, n = days), tail(sc, n = days), tail(sd, n = days), tail(se, n = days), tail(sg, n = days), tail(sh, n = days), tail(si, n = days), tail(sj, n = days), tail(sk, n = days), tail(sl, n = days))


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
#######################################################
#
COMcement <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMplastic <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMtextile <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMmachinery <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMelectrical <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMbiotech <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMchemical <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMglass <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMpaper <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMmetal <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMrubber <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMcar <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMcomputer <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMsemiconductor <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMecomponents <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMelectronics <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMcnetwork <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMinfservice <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMconstruction <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMshipping <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMtourism <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMbanking <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMinsurance <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMfinancial <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMtrade <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMechannel <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMsecurities <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMother <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMO.E.G <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
ecommerce <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMagricultural <- function(){
  library(quantmod)
  getSymbols('AAPL')
  getSymbols('FB')
  getSymbols('AME')
  getSymbols('CCF')
  getSymbols('PPT')

  S1 = data.frame(AAPL)
  S2 = data.frame(FB)
  S3 = data.frame(AME)
  S4 = data.frame(CCF)
  S5 = data.frame(PPT)

  names = gsub("^FB\\.(.*$)", "\\1", names(FB))
  names(S1) = names
  names(S2) = names
  names(S3) = names
  names(S4) = names
  names(S5) = names

  # rbind into one dataframe
  S1$label = "S1"
  S2$label = "S2"
  S3$label = "S3"
  S4$label = "S4"
  S5$label = "S5"


  df = rbind(S1, S2, S3, S4, S5)


  # Packages
  library(ggplot2)
  library(directlabels)

  # The plot - labels at the beginning and the ends of the lines.
  #ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
  #  geom_line()  +
  #  scale_colour_discrete(guide = 'none')  +
  #  geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))

  GG <- ggplot(df, aes(as.Date(rownames(df)), Adjusted, group = label, colour = label)) +
    geom_line()  +
    scale_colour_discrete(guide = 'none')  +
    geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points")))
  return(GG)
}
COMsemiconductor()
