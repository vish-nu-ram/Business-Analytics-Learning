
housing <-- read.csv("BostonHousing.csv")
housing$MEDV

reg <- lm(housing$MEDV ~ housing$CRIM + housing$CHAS + housing$RM , Data  = housing)
summary(reg)

heatmap(cor(housing))
