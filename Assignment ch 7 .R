# 7.2
library(caret)
library(class)
universal_data <- read.csv("UniversalBank.csv")

summary(universal_data)
head(universal_data)

Education_1 <- ifelse(universal_data$Education == 1, 1,0)
Education_2 <- ifelse(universal_data$Education == 2, 1,0)
Education_3 <- ifelse(universal_data$Education == 3, 1,0)

universal_data_new <- cbind(universal_data, Education_1,Education_2,Education_3)

universal_data_final<- universal_data[, -c(1,5,8,10)]

train_index <- sample(5000,5000*0.6,replace = FALSE)

train_univ <- universal_data_final[train_index,]
valid_univ <- universal_data_final[-train_index,]

train_outcome <- universal_data_new$Personal.Loan[train_index]
valid_outcome <- universal_data_new$Personal.Loan[-train_index]

head(valid_univ)

knn(train = train_univ, test = valid_univ ,cl = train_outcome , k = 1)

results <- c()
for( i in 1:10)
{
  model <- knn(train = train_univ, test = train_univ ,cl = train_outcome , k = i)
  model2 <- knn(train = train_univ, test = valid_univ ,cl = train_outcome , k = i)
  tab <- table(valid_outcome, model2)
  tab2 <- table(train_outcome, model)
  results[i,] <- c(accuracy(tab), accuracy(tab2))
  
}
