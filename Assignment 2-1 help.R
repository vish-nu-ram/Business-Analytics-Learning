
housing <-- read.csv("BostonHousing.csv")
housing$MEDV

reg <- lm(housing$MEDV ~ housing$CRIM + housing$CHAS + housing$RM , Data  = housing)
summary(reg)

heatmap(cor(housing))
housing

step(reg, direction = "backward")


step(reg, direction = "forward")

step(reg, direction = "both")


appl <- read.csv("ApplianceShipments.csv")
appl
appl_ts <- ts(data = appl, start=c(1985,1), end = c(1989,4),frequency = 4 )

plot(appl_ts)



canw <- read.csv("CanadianWorkHours.csv")
canw
canw_ts <- ts(data = canw, start=c(1966,1), end = c(2000,1),frequency = 1 )
canw_ts
plot(canw_ts)

sov <- read.csv("SouvenirSales.csv")
sov
sov_ts <- ts(data = sov, start=c(1995,1), end = c(2001,12),frequency = 1 )
sov_ts
plot(sov_ts)


sov$Sales <- log(sov$Sales)
sov_ts <- ts(data = sov, start=c(1995,1), end = c(2001,12),frequency = 1 )
sov_ts
plot(sov_ts)
