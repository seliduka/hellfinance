library(ggplot2)
library(reshape)
library(tidyr)
library(dplyr)


set.seed(78)
vector <- runif(n = 360, min = -4, max = 4)
dates <- seq(as.Date('1980-01-01'), length.out=360, by='1 month')


df <- data.frame(dates = dates, index = vector)

#Aqui ya se hace la diferencia entre positivo y negativo con color
df$colour <- ifelse(df$index < 0, "Negative","Positive")
ggplot(df,aes(dates,index,label=""))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(name = "Valor", values=c(Positive="firebrick",Negative="dodgerblue4"))+
  ggtitle("Index")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year") + ylab("S.D.")
