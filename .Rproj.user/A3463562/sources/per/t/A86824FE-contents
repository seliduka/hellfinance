COMpanel <- function(days){
library(quantmod)
getSymbols("2323.tw")
getSymbols("2340.tw")
getSymbols("2349.tw")
getSymbols("2374.tw")
getSymbols("2406.tw")
getSymbols("2409.tw")
getSymbols("2426.tw")
getSymbols("2438.tw")
getSymbols("2466.tw")
getSymbols("2486.tw")
getSymbols("2489.tw")
getSymbols("2491.tw")
getSymbols("3008.tw")
getSymbols("3019.tw")
getSymbols("3024.tw")
getSymbols("3031.tw")
getSymbols("3038.tw")
getSymbols("3049.tw")
getSymbols("3050.tw")
getSymbols("3051.tw")
getSymbols("3059.tw")
getSymbols("3149.tw")
getSymbols("3356.tw")
getSymbols("3383.tw")
getSymbols("3406.tw")
getSymbols("3437.tw")
getSymbols("3454.tw")
getSymbols("3481.tw")
getSymbols("3504.tw")
getSymbols("3535.tw")
getSymbols("3543.tw")
getSymbols("3563.tw")
getSymbols("3576.tw")
getSymbols("3591.tw")
getSymbols("3622.tw")
getSymbols("3669.tw")
getSymbols("3673.tw")
getSymbols("3714.tw")
getSymbols("4934.tw")
getSymbols("4935.tw")
getSymbols("4942.tw")
getSymbols("4956.tw")
getSymbols("4960.tw")
getSymbols("4976.tw")
getSymbols("5234.tw")
getSymbols("5243.tw")
getSymbols("5484.tw")
getSymbols("6116.tw")
getSymbols("6120.tw")
getSymbols("6131.tw")
getSymbols("6164.tw")
getSymbols("6168.tw")
getSymbols("6176.tw")
getSymbols("6209.tw")
getSymbols("6225.tw")
getSymbols("6226.tw")
getSymbols("6278.tw")
getSymbols("6289.tw")
getSymbols("6405.tw")
getSymbols("6431.tw")
getSymbols("6443.tw")
getSymbols("6456.tw")
getSymbols("6477.tw")
getSymbols("6668.tw")
getSymbols("6706.tw")
getSymbols("8104.tw")
getSymbols("8105.tw")
getSymbols("8215.tw")


S1 = data.frame(`2409.TW`)
S2 = data.frame(`3049.TW`)
S3 = data.frame(`3481.TW`)
S4 = data.frame(`6116.TW`)
S5 = data.frame(`8105.TW`)

names = gsub("^........(.*$)", "\\1", names(`2409.TW`))
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

df = rbind(tail(S1, n = days), tail(S2, n = days), tail(S3, n = days), tail(S4, n = days), tail(S5, n = days))

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
COMfood <- function(){
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

