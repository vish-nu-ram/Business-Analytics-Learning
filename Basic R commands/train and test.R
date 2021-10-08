rm(list=ls(all=T))
data=read.csv("Data.csv",header=T)
dim(data)
save.image("Data.RData")
rm(list=ls(all=T))
load("Data.RData")
nrow(data)

#Data Split
set.seed(123)
trainRows=sample(1:nrow(data),nrow(data)*0.7)
df_train=data[trainRows,]
df_val=data[-trainRows,]

rm(list=ls()[ls()!='data'])
ls()
#Using Caret
library(caret)
names(data)
data=subset(data,select=-c(ID,ZIP.Code))

index <- createDataPartition(data$Personal.Loan, p=0.70,list=FALSE)
df_train <- data[ index,]
df_val <- data[-index,]

preProcValues <- preProcess(df_train, method = c("center", "scale"))
df_trainTransformed <- predict(preProcValues, df_train)
head(df_trainTransformed,2)
head(scale(df_train),2)
df_valTransformed <- predict(preProcValues, df_val)

rm(list=setdiff(ls(),c('df_trainTransformed','df_valTransformed')))
save.image("Data.RData")
