library(tidyverse)
install.packages("readxl")
library(readxl)
df <- read_excel("Bank Marketing Data.xlsx")

df <- as.data.frame(df)

df.yes <- df[ sample(which ( df$y == "yes" ) ,500), ]
df.no <- df[ sample(which ( df$y == "no" ) ,500), ]
attach(df)

t.test(duration ~ y)

t.test(duration[y=='yes'],duration[y=='no'])


numeric.df <- df[,c(1,6,10,12,13,14,15,17)]
numeric.df$y <- ifelse(numeric.df$y =="yes",1,0)

numeric.df
plot(heatmap(cor(numeric.df)))
cor(numeric.df)
