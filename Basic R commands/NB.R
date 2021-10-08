library(e1071)
## Categorical data only
data<-read.csv("NB_Example.csv",header=T)
data
model <- naiveBayes(Class ~ ., data = data)
model
