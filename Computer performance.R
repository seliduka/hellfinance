#library(ggplot2)
#set.seed(5)
#diamonds.subset <- diamonds[sample(nrow(diamonds), 100), ]#
#
#qplot(carat, data = diamonds, geom = "histogram",
#      fill = color)


df <- data.frame("item" = 1:300, "num" = 1:300, "group" = 1:300)
df$item[1:100] <- "CPU"
df$item[101:200] <- "Memory"
df$item[201:300] <- "Hard_Disk"
df$num[1:100] <- 1:100
df$num[101:200] <- 1:100
df$num[201:300] <- 1:100
df$group[1:89] <- "3.high"
df$group[90:99] <- "2.medium"
df$group[99:100] <- "1.low"
df$group[101:189] <- "3.high"
df$group[190:199] <- "2.medium"
df$group[199:200] <- "1.low"
df$group[201:289] <- "3.high"
df$group[290:299] <- "2.medium"
df$group[299:300] <- "1.low"


# install.packages("ggplot2")
library(ggplot2)

ggplot(df, aes(x = item, use = `num` , fill = group)) +
  geom_bar()


plot06<-ggplot(df, aes(x=item, fill=group)) + geom_bar(
  width = 0.5, position ="stack") + geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
color = I("white"), size = 3)

plot06



# install.packages("ggplot2")
library(ggplot2)

ggplot(df, aes(x = item, use_ratio = `num`, fill = group)) +
  geom_bar(stat = "identity")

# install.packages("ggplot2")
library(ggplot2)

ggplot(df, aes(x = item, y = `num`, fill = group)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer()

