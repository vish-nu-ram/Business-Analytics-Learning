data("mtcars")
DF<-mtcars
#to remove variables from the environment we use rm function
rm(mtcars)
#Lets observe the structure of the data
str(DF)

#Summary of the data
summary(DF)


DF[10:15,1]


#### Subsetting Dataframes
# Subsetting using []
# Using a subset function
DF_s1<-DF[c(1,6,7)] #Selecting 1st 6th and 7th colum

x<- c("mpg","wt","qsec")
DF_s2<-DF[names(DF)%in%x]

##### Exercise
# Take subset of all those columns that are not there in x
x<- c("mpg","wt","qsec")
DF_s3<-DF[!(names(DF) %in% x)]
names(DF_s3)

#Select all the cars that have mileage over 25 and only mph and cyl columns
DF_s6<-DF[DF$mpg>25,c("mpg","cyl")] #Instead of column names we can also give indices
dim(DF_s6)


##### Using subset
# subset(what is to be subset, on what condition, what should be selected)
# Condition is not necessary to select columns but is necessary to select rows

DF_s4<-subset(DF,select=c(mpg,cyl)) #Observe that condition is not mentioned and we got the required columns
names(DF_s4)
dim(DF_s4)
##Select all the rows for mpg and cyl whose mpg is greater than 25
DF_s5<-subset(DF,mpg>25,select=c(mpg,cyl))
dim(DF_s5)

row.names(DF)
x=c("Honda Civic","Toyota Corolla")
DF[x,]
subset(DF,row.names(DF)%in%x)
row.names(DF)%in%x
(1:nrow(DF))[row.names(DF)%in%x]
intersect(row.names(DF),x)


##### Lets work with the mileage column.. and study conditionals, loops.
#Create a new variable as "condition" that if mileage is less than 25 "Bad" else "Good"
#For this task we use a function ifelse

head(DF,3)
DF$condition<-ifelse(DF$mpg>25,"Good","Bad")
head(DF,3)
DF$condition

##### Ifelse can be nested
# mpg>25 is good, mpg>18 is ok and mpg<18 is bad
 
DF$condition<-ifelse(DF$mpg>25,"Good",ifelse(DF$mpg>18,"OK","Bad"))
DF$condition

subset(DF,condition=="Good")
DF[DF$condition=="Good",]
DF[(DF$condition=="Good")|(DF$condition=="OK"),]
DF[DF$condition%in%c("Good","OK"),]

##### If we had to do this using a loop

for(i in 1:nrow(DF)){
  if(DF$mpg[i]>25){
    DF$condition[i]<-"Good"
  }
  else{
    DF$condition[i]<-"Bad"
  }
}
####
for(i in 1:nrow(DF)){
  if(DF$mpg[i]>25){
    DF$condition[i]<-"Good"
  }
  else 
    if(DF$mpg[i]>18){
      DF$condition[i]<-"OK"
    }
  else{
    DF$condition[i]<-"Bad"
  }
  
}

DF$condition


#### Writing a custom function in R
#Lets create a simple functions to check if a number is even or odd

even_odd<-function(x){
  if(x%%2==0){
    return("even")
  }
  else{
    return("odd")
  }
  
}

even_odd(20)

sapply(c(20,21),even_odd)

even_odd_vec<-function(x){
  return(ifelse(x%%2==0,"even","odd"))
}
even_odd_vec(c(20,21))

add<-function(x){
  return(sum(x))
}
v=c(10,20,30)
add(v)
even_odd(add(v))


even_odd(as.integer(readline(prompt="Enter an integer: ")))

n=readline(prompt="Enter an integer: ")
n

print(readline(prompt="Enter an integer: "))

Addition<-function(x,y){
  return(x+y)
}
Addition(100,200)


#### While loop
i = 1
while (i < 6){
  print(i)
  i = i+1
}


#### next and break statements
x = 1:5

for (val in x) {
  if (val == 3){
    break
  }
  print(val)
}

# next statement

for (val in x) {
  if (val == 3){
    next
  }
  print(val)
}

##### Exercise
#Create a function to obtain the sum of elements of a vector. 
#The argument that the function takes is vector
#Create a function that takes two arguments a,b 
#and returns a raised to power b


additon<-function(x){
  return(sum(x))
}

A=1:10
additon(A)

pow<-function(x,y){
  return(x^y)
}

# Multiple Returns
# The return() function can return only a single object. If we want to return multiple values in R, we can use a list (or other objects) and return it.
multiReturn = function() {
  myList <- list(1, 20, c("a","b"))
  return(myList) 
}

multiReturn()


#### Lists
#A list is datastructure that can store multiple data structures
val=c(1,7,9)
ch=c("A","X","Z")
A<-list(val,ch,data.frame(val,ch),list(val,ch))

#To access the elements of the list
A[1] #To get the first element of the list which is val. But we know that val has 3 elements in it
A[[1]][1] # To get the first element of the val

A[[3]]$ch #Since the third element of the list is a dataframe we can access the elements of the dataframe using $

##elements of list can also be accessed using names if they have
B=list(M=val,N=ch,O=data.frame(val,ch),P=list(val,ch))
B$O[[1]]
#To unlist the elements
unlst<-unlist(B)
unlst

#### Subsetting a data frame
# By indices
# By column names
names(DF)
DF_numeric<-DF[c(1,3:6)]


### The "Apply" family
#Used to manipulate slices of data in a repetitive way

##### Apply
#Syntax apply(on what, by row or by column, what function to be applied)

#Lets say in the num_DF, we need to find mean value for each of the columns. This is a repetitive process that can be quickly done by apply
apply(DF_numeric,2,mean)

#Although it is not meaningful in this case, if we want to take mean of each row
apply(DF_numeric,1,mean)

#We can also apply custom functions on the data. Lets say we want to take log of x for each value
flog<-function(x){
  return(log(x))
}
apply(DF_numeric,2,flog)


##### Excerise
# Write a function that takes a vector to identify how far a value is away from its mean. Use apply function to apply it on DF_numeric

f_dist<-function(x){
  return(x-mean(x))
}

apply(DF_numeric,2,f_dist)



##### tapply
# We have a data thaqt can be divided into groups
# On each of this group we want to apply the function and want to get a table as output.
# We use tapply

#Lets us consider mpg and cyl of DF data, we want to find the mean mileage for each of the group.
#Observe the data type of mpg and cyl
str(DF$mpg)
str(DF$cyl)
#Since cyl is interpreted as numeric lets covert that into categorical
DF$cyl<-as.factor(as.character(DF$cyl)) #Observe this syntax 
str(DF$cyl) #Observe that the data type is converted to factor i.e. categorical

#Now for each of the cyl group lets compute the mean
tapply(DF$mpg,DF$cyl,FUN=mean)

##### sapply and lapply
#Both work in a similar way, traverse over a vector or list and calling the function for each item except that sapply simplifies the output 
sapply(DF_numeric,mean)
lapply(DF_numeric,mean)
sapply(DF_numeric,mean,simplify = F)


#### Working with Factors/Categories
#Factors have levels
#Factors created when we read non-numerical columns into a data frame.

x <- factor(c("A","B","C","A"))
str(x) #Observe that individual elements are stored as numbers

#To get the levels of the variable
levels(x)
###### Here is an exercise
#Create a numeric variable A with 5 values in it
#Create a categorical variable B with 4 levels in it "A","B","C" and 5 elements
#Replace the 5th element of variable A with 1000
#Replace the 5th element of Variable B with "Alpha"


A<-c(1,2,3,4,5)
B<-factor(c("A","A","B","C","B"))
A
B
A[5]<-1000
A
B[5]<-"Alpha"
B


##### How to modify a factor
levels(B)<-c(levels(B),"Alpha")
B[5]<-"Alpha"
B


##### Exercise
#Create a factor vector with levels "A","B","C" with 20 elements
#All the elements with level B are to be converted to "Beta"

X<-factor(sample(c("A","B","C"),20,replace = T))
X
levels(X)[levels(X)=="B"]<-"Beta"
X


#subsetting the data
#Column names wise 
Data<-read.csv("SampleData.csv",header=T,sep=",")
names(Data)
subset<-subset(Data,select=c("Student.id","English1"))
head(subset)
subset<-subset(Data,select=-c(Student.id,English1))
head(subset)
#Data[rows,cols]
Subset<-Data[1:5,] #selects first five records from all the columns
Subset<-Data[,2] # selects all rows from 2nd column
Subset<-Data[c(1,3),] #what is the output?
Subset<-Data[,c(1:2,5)] #what is the output?
#By conditions
Subset<-subset(Data,Data$Math1>60) #Select all records that have math marks greater than 60 
Subset<-subset(Data,Data$Math1>60,select=c(1:3)) #selects data where math marks greater than 60 from first 3 columns
Subset<-Data[which(Data$Math1>50 & Data$Science1>70),] # only those records than has math>50 and science>70
Subset<-Data[Data$Student.id != "1",] #selects all data except for the student 1.
Subset<-Data[Data$Student.id == Data$Student.id[1:5],]


V=Data$OverallPct1
mean(V)
median(V)
var(V)
sd(V)
summary(V)
boxplot(V)
hist(V)
quantile(V)
quantile(V,0.1)


## datasets to understands the major differences between the left join
## right joins
A=data.frame(Name=c("Alpha","Beta","Gamma","Delta"), Age=c(24,25,23,28))
B=data.frame(Name=c("Alpha","Gamma","Zeta","Psi"),Edu=c("M","D","B","H"))
MergedData1<-merge(A,B,
                   by.x="Name",by.y="Name",
                   all.x=TRUE) #left (outer) join
MergedData1
MergedData2<-merge(A,B,
                   by.x="Name",by.y="Name",
                   all.y=TRUE) #right (outer) join
MergedData2
MergedData3<-merge(A,B,
                   by.x="Name",by.y="Name",
                   all=TRUE) #Full (outer) join
MergedData3
