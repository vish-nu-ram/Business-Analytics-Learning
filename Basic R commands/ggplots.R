###########Visualizations###########
#When to Use what by looking at the data
#1. Box plot: To understand the distribution of a numerical attribute thoroughly
#             including the extreme value analysis


#2. Bar plot: For a categorical attribute, for each of the levels of category,
#             getting the counts/frequency. 

#3. Histogram: For a numeric/continuous variable, to understand the distribution

#4. Scatterplot: To find relation between two numeric/continuous attributes

#5. Grouped boxplots: For a relation between a categorical and continuous variable 
#                     To thoroughly understand distribution across levels of a category


data<-read.csv("Data.csv",header=T)
#####Boxplots####
#1. How is income of customers of the banks distributed
boxplot(data$Income)


#2. Giving a title to this boxplot and name the axes
boxplot(data$Income,main="Distribution of Income",
        xlab="Income",ylab="Income")


#To do the same using ggplot
library(ggplot2)
ggplot(data, aes("",Income))+
  geom_boxplot()

#In ggplot we need aesthetics both x and y. Since we have to study only variable
#income, we give a character/string value to x and this solves the problem

###Beautification Part
#Lets give a title to this plot
ggplot(data, aes(x="",y=Income))+
  geom_boxplot()+
  ggtitle('Distribution of Income')

#This title is not to the center. Also every time typing code is painful. So
#lets save the basic plot
p<-ggplot(data, aes(x="",y=Income))+
  geom_boxplot()+
  ggtitle('Distribution of Income')

#We can also change the background. Lets use another theme
p+theme_classic()
p+theme_bw()
p+theme_dark()

#Create a bold title
p+theme(plot.title = element_text(face='bold'))

#The title is not to the centre. Lets make it to centre 
p+theme(plot.title = element_text(face='bold',hjust = 0.5))

#Lets remove the background and put the x and y axis
p+theme(plot.title = element_text(face='bold',hjust = 0.5),
        rect = element_blank(),axis.line = element_line())

##There are many more parameters to play with, this is just an
##introduction. You can experiement with these parameters while working on the data

##Grouped boxplots
plot(data$Family,data$Income)

#In ggplot
ggplot(data,aes(Family,Income))+
  geom_boxplot()+
  ggtitle('Income Vs Family')+
  theme_classic()

#####Histogram#####

##Create a histogram
#Simple histogram using base graphics
hist(data$Income)

#Adjusting the bin widths
hist(data$Income,breaks =20,main="Histogram with 20 Breaks",xlab="Income")
hist(data$Income,breaks =5,main="Histogram with 5 Breaks",xlab="Income")

#You might have observed that,bins decide the granularity

##Simple histogram in ggplot
ggplot(data,aes(x=Income))+
  geom_histogram()

ggplot(data,aes(x=Income))+
  geom_histogram(binwidth=50)


##This image is not clear right... lets make it better
#Lets clear the background
ggplot(data,aes(x=Income))+
  geom_histogram(binwidth=50)+
  theme_classic()

ggplot(data,aes(x=Income))+
  geom_histogram(binwidth = 10,
                 colour='black',
                 fill='white')+
  theme(axis.line = element_blank())
  

#####Bar plots#####

#How many records corresponding to each family size available
data$Family<-as.factor(as.character(data$Family))
data$Personal.Loan<-as.factor(as.character(data$Personal.Loan))

#Barplot in basic plots
p=plot(data$Family) #Since it is categorical, frequency for each level is plotted
text(p,table(data$Family),labels=as.vector(table(data$Family)))

#Writing the frequency counts and using barplot function
p<-barplot(table(data$Family))
text(p,table(data$Family),
     labels=as.vector(table(data$Family)))


##Barplots in ggplot
ggplot(data,aes(x=Family))+
  geom_bar()


#

#Display counts on the plots
ggplot(data,aes(x=Family))+
  geom_bar()+
  geom_text(stat="count",aes(label=..count..))+
  theme_classic()

##Colour to represent another attibute
ggplot(data,aes(x=Family,fill=Personal.Loan))+
  geom_bar()+
  geom_text(stat="count",aes(label=..count..))+
  theme_classic()


#####Scatterplots######

##Scatterplots are a way to understand if there is any relation
##between two numeric attributes
plot(data$Age,data$Experience)

#How to plot this scatter plot using ggplot
ggplot(data,aes(x=Age,y=Experience))+
  geom_point()
ggplot(data,aes(x=Age,y=Income))+
  geom_point()

###In all the above plots we used either one or two variables.
##How to add more than 2 variables in a ggplot


#####Multidimensional plot#####
#For example find the relation between age and income with personal loan and mortgage
ggplot(data,aes(x=Age,y=Income))+
  geom_point(aes(color=Personal.Loan,size=Mortgage))+
  theme_classic()





