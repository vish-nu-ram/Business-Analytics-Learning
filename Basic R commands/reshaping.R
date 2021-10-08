rm(list=ls(all=TRUE))
#importing data
data<-read.csv("SAMPLE1.csv")
data

#checking the structure of data
str(data)

#merging the data and time into one column
data$datetime<-as.POSIXct(paste(data$information_date, data$information_time), format="%d-%m-%Y %H:%M")
#checking the data
head(data)

#removing the extra date and time columns
data$information_date<-NULL
data$information_time<-NULL
#checking the data
data


#Reshaping the data 
data1<-reshape(data, idvar = "datetime", timevar = "client_id")
#checking the data
data1
