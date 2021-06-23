getOption('timeout')
options(timeout=1000)
#getSymbols("3443.tw")

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
  icdesign = c("3006.TW", "3014.TW","3035.TW", "3545.TW","3588.TW","4961.TW")
  getSymbols(icdesign)
#  sa<- icdesign[1]"2363.TW"
#  sb<- icdesign[2]"2379.TW",

#  sd<- icdesign[2],"2401.TW",
#  sf<- icdesign[6],"2454.TW"
  sh<- icdesign[1]
  si<- icdesign[2]
  sk<- icdesign[3]
  so<- icdesign[4]
  sp<- icdesign[5]
  sq<- icdesign[6]


#  sa = data.frame(get(sa))
#  sb = data.frame(get(sb))

#  sd = data.frame(get(sd))
#  sf = data.frame(get(sf))
  sh = data.frame(get(sh))
  si = data.frame(get(si))

  sk = data.frame(get(sk))
  so = data.frame(get(so))
  sp = data.frame(get(sp))
  sq = data.frame(get(sq))
  names = gsub("^........(.*$)", "\\1", names(`3006.TW`))
#  names(sa) = names
#  names(sb) = names

#  names(sd) = names
 # names(sf) = names
  names(sh) = names
  names(si) = names
  names(sk) = names
  names(so) = names
  names(sp) = names
  names(sq) = names


  # rbind into one dataframe
#  sa$label = "sa"
#  sb$label = "sb"

#  sd$label = "sd"
 # sf$label = "sf"
  sh$label = "晶豪科"
  si$label = "聯陽"
  sk$label = "智原"
  so$label = "敦泰"
  sp$label = "通嘉"
  sq$label = "天鈺"
  df = rbind(tail(sh, n = days), tail(si, n = days), tail(sk, n = days),tail(so, n = days),tail(sp, n = days),tail(sq, n = days))


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

#getSymbols(food)
COMfood <- function(days){
  library(quantmod)
  food = c("1201.TW", "1203.TW", "1210.TW", "1213.TW", "1215.TW", "1216.TW", "1217.TW", "1218.TW", "1219.TW", "1220.TW", "1225.TW", "1227.TW", "1229.TW", "1231.TW", "1232.TW", "1233.TW", "1234.TW", "1235.TW", "1236.TW")
  getSymbols(food)
  sc = food[3]
  se = food[5]
  sm = food[13]
  sn = food[14]
  so = food[15]
  sr = food[18]

  sc = data.frame(get(sc))
  se = data.frame(get(se))
  sm = data.frame(get(sm))
  sn = data.frame(get(sn))
  so = data.frame(get(so))
  sr = data.frame(get(sr))
  names = gsub("^........(.*$)", "\\1", names(`1201.TW`))

  names(sc) = names
  names(se) = names
  names(sm) = names
  names(sn) = names
  names(so) = names
  names(sr) = names


  # rbind into one dataframe

  sc$label = "大成"
  se$label = "卜蜂"
  sm$label = "聯華實業"
  sn$label = "聯華食品"
  so$label = "大統益"
  sr$label = "興泰"
  df = rbind(tail(sc, n = days),tail(se, n = days),tail(sm, n = days),tail(sn, n = days),tail(so, n = days),tail(sr, n = days))


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
##############################################
COMcement <- function(days){
  library(quantmod)
  cement = c("1101.TW", "1102.TW", "1103.TW", "1104.TW", "1108.TW", "1109.TW", "1110.TW")
  getSymbols(cement)
  sa = cement[1]
  sb = cement[2]
  sc = cement[3]
  se = cement[5]
  sf = cement[6]

  sa = data.frame(get(sa))
  sb = data.frame(get(sb))
  sc = data.frame(get(sc))
  se = data.frame(get(se))
  sf = data.frame(get(sf))

  names = gsub("^........(.*$)", "\\1", names(`1101.TW`))

  names(sa) = names
  names(sb) = names
  names(sc) = names
  names(se) = names
  names(sf) = names


  # rbind into one dataframe

  sa$label = "台泥"
  sb$label = "亞泥"
  sc$label = "嘉泥"
  se$label = "信大"
  sf$label = "東亞"

  df = rbind(tail(sa, n = days),tail(sb, n = days),tail(sc, n = days),tail(se, n = days),tail(sf, n = days))

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
###########################################
COMplastic <- function(days){
  library(quantmod)
  plastic = c("1301.TW", "1303.TW", "1304.TW", "1305.TW", "1307.TW", "1308.TW", "1309.TW", "1310.TW", "1312.TW", "1313.TW", "1314.TW", "1315.TW", "1321.TW", "1323.TW", "1324.TW", "1325.TW", "1326.TW", "1337.TW", "1340.TW", "1341.TW", "1342.TW", "4306.TW")
  getSymbols(plastic)
  sa = plastic[1]
  sb = plastic[2]
  sc = plastic[3]
  sd = plastic[4]
  sf = plastic[6]
  sl = plastic[12]
  sp = plastic[16]
  st = plastic[20]

  sa = data.frame(get(sa))
  sb = data.frame(get(sb))
  sc = data.frame(get(sc))
  sd = data.frame(get(sd))
  sf = data.frame(get(sf))
  sl = data.frame(get(sl))
  sp = data.frame(get(sp))
  st = data.frame(get(st))

  names = gsub("^........(.*$)", "\\1", names(`1301.TW`))

  names(sa) = names
  names(sb) = names
  names(sc) = names
  names(sd) = names
  names(sf) = names
  names(sl) = names
  names(sp) = names
  names(st) = names

  # rbind into one dataframe

  sa$label = "sa"
  sb$label = "sb"
  sc$label = "sc"
  sd$label = "sd"
  sf$label = "sf"
  sl$label = "sl"
  sp$label = "sp"
  st$label = "st"

  df = rbind(tail(sa, n = days),tail(sb, n = days),tail(sc, n = days),tail(sd, n = days),tail(sf, n = days),tail(sl, n = days),tail(sp, n = days),tail(st, n = days))

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
#################################################

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
