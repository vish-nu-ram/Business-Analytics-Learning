#Agenda
# Date handling in R
# Reshape (casting and melt) 
# dplyr library introduction
# R Markdown introduction

rm(list=ls(all=T))
####Dates and Reshape
data<-read.csv("Product_Purchase_Info.csv",header = T)
data$Product<-as.factor(as.character(data$Product))
data$Date<-as.character(data$Date)
str(data$Date)
data$Date<-gsub("22017","2017",data$Date)
str(data$Date)
library(lubridate)
data$Date<-strptime(data$Date,format="%d-%m-%Y")
data$month<-month(data$Date)
tapply(data$Price,data$month,sum)

library(reshape2)
data2<-dcast(data,month~Product,value.var ="Price",fun.aggregate = sum)
data2$Revenue=apply(data2[2:4],1,sum)
data2
data3<-data2[-5]
data4<-melt(data3,id.vars = "month",variable.name = "product")
data4

library(ggplot2)
ggplot(data2,aes(x=month,y=Revenue,fill=Revenue))+
  geom_bar(stat="identity")

rm(list=ls(all=T))
# dplyr library introduction
library(dplyr)
#Five verbs of data manipulation in R using dplyr: select, mutate, filter, arrange 
#                                                 and summarise
#hflights is a dataset in library hflights
library(hflights)
data(hflights)

#Converting the data into local dataframe using tbl
flights=tbl_df(hflights)
rm(hflights)
head(flights)

#####filter: Keep rows matching criteria####
#lets say we want to extract all flights on january first
jan1_flight=filter(flights, Month==1, DayofMonth==1)

#We can also give multiple conditions
y=filter(flights, Month==1 & UniqueCarrier=="AA")

####Select: pick columns####
z=select(flights,DepTime,ArrTime)
z
z=select(flights,DepTime:TailNum)
z
z=select(flights,-c(DepTime,TailNum))
z
z=select(flights, Year:DayofMonth, contains("Taxi"), contains("Delay"))
z

#### Arrange: ordering rows####
x=arrange(flights,DepDelay)
x
x=arrange(flights,desc(DepDelay))
x

#### Mutate: Creating New variabes ####
mutate(flights,Speed = Distance/AirTime*60)
flights<-mutate(flights,Speed = Distance/AirTime*60)

#### Summarise #### 
# Lets say for each destination, we would like to calculate average delay
x=group_by(flights,Dest) %>%
  summarise(avg_delay = mean(ArrDelay, na.rm=TRUE))
x
##Summarise_each allows for multiple column summarization at once
flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(mean), Cancelled, Diverted)

flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(mean,median), Cancelled, Diverted)

flights %>%
  group_by(UniqueCarrier) %>%
  summarise_all(funs(mean))

# for each day of the year, count the total number of flights and 
#sort in descending order
flights %>%
  group_by(Month, DayofMonth) %>%
  summarise(flight_count = n()) %>%
  arrange(desc(flight_count))

#Reading file from excel
install.packages("xlsx")
library("xlsx")
data<-read.xlsx("Product_Purchase_Info.xlxs", sheetIndex=1, header=TRUE)
write.xlsx(data, "data.xlxs", sheetName="Sheet1",  col.names=TRUE, row.names=FALSE, append=FALSE)
write.xlsx(data, "data.xlxs", sheetName="Sheet2",  col.names=TRUE, row.names=FALSE, append=TRUE)

