## A Brief introduction to R

# First there was "S" and then there is "R".
# S was developed by John Chambers and colleagues at AT&T Bell labs and 
# R by Ross Ihaka and Robert Gentleman at University of Auckland
# R is a free open source software licensed under the GNU general public license (GPL 2)

## R Studio
#RStudio is an integrated development environment (IDE) for R that provides an alternative interface to R that has several advantages over other the default R interfaces

##############################Basic calculations##############################               
#Mathematical Operations
#Addition
235+345
#Subtraction
11-6
#Multiplication
12*4
#Division
12/4
#Modulo division
13%%4
#Integer division
13%/%4
#Exponentiation
2^3
2+2^2
(2+2)^2
#Mathematical Functions
sqrt(2)
log(2)

############################## Assigning variables in R ##############################
# The following can be used to assing value to variables
# "<-"
# "->"
# "="
# General convention is to use "<-" for assignment
# After creating a variable observe that the created variable is in Environment window.
#Assigning 2 to variable A and print A
A<-2
A 

############################## Atomic Objects in R ##############################
#R has the following datatypes
# Numeric/integer
# Character
# Logical
# Complex
#To check the datatype of the variable, we use a function called "class".  

#Integer or numeric
A<-213
class(A)
A<-213L
class(A)
#Character- always in quotes (single or double)
A<-'INSOFE'
class(A)
#Logical
A<-TRUE
class(A)
# Complex
A<-2+3i
class(A)


############################## List of Relational and Logical operators in R ##############################
#### Relational operators
# "==" is checking for equality
# ">=" is greater than or equal to
# "<=" is less than or equal to
# "<" is less than
# ">" is greater than
# "!=" is not equal to
#The output when these operators are used is Logical i.e. whether the relation is true or false

#### Logical operators
# "|" or "||" is "OR" operator "|" is vectorised while "||" is not
# "&" or "&&" is "AND" operator "&" is vectorised while "&&" is not
# "!" is NOT operator
#Lets do some operations on numeric values and observe the output
A=10
B=20
A==B
A<=B
A>=B
A!=B

###Lets build some not so very complex expressions using both relational and logical operators
A=10
B=15
A==B || A<B
A==B && A<B
!(A==B || A<B) 

############################## Data Structures in R ##############################
# Vectors
# Matrices
# Data Frames
# Lists
#A vector is an object that consists of elements of same data type.Lets see how to create a vector using a c function. c is a generic function that combines all its arguments

#Creating a numeric vector
A<- c(1,7,9)
A

#Creating a character vector
A<-c("x","y","INSOFE")
A
#Creating a logical vector
A<-c(T,F,F,T)
A

#If we want to create numbers in sequence we can use a ":" operator

A<-1:10
A

#To get sequence of alphabets
letters[1:10]
LETTERS[1:10]

#To create a empty vector of numeric type
B<-vector(mode="numeric")
B

### How to extract elements from a vector
#Let's create a vector A with 5 values and we need to extract 3 value from it

A<-c(1,11,33,22,7)
A[3] #Observe the bracket type-- For functions we use parenthesis and for variable subsetting we use square brackets

#What if we want to get first and 5th element from the vector
A[c(1,5)]

#Now we have idea about vectors and operators lets do a small exercise together
#Create a variable A with elements 1,17,14,7,4,-3,18,9,3.5, 2.8 
#How many elemnts are greater than 9
#Extract all those elements that are greater than 9

A<-c(1,17,14,7,4,-3,18,9,3.5, 2.8)
A>9 #Observe the output. 
#Since we need to subset from variable a, all the elements greater than 9, we do the following
A[A>9]
#Take some time to understand this subsetting
#To create a sequence of vectors, we have seen a ":" operartor. Lets see another way by using "seq" function

#seq is a function that takes parameters from, to, and by
A<-seq(1,20,by=2)
A

#### Vector operations
# "+" 
# "-"
# "*"
# "/"

A=c(1:10)
B=(11:20)
A
B
#Vector operations
A+B
B-A
A*B
A/B 
#Observe that the operations are performed element wise

### Matrices
# These are arrays of two dimensions or more but we focus on 2d
# The data type for each of these elements should be same as in the case of vectors
#Matric is a function to create a matrix. It has the following arguments - what are the elements in the matrix, how many rows in matrix, how many columns, and how should the filling of elements is done
A<-matrix(1:6,nrow=3,byrow = T)
A

B<-matrix(1:6, nrow=3,byrow=F) #Observe that we haven't mentioned ncol as it would be redundant
B

#Extracting elements out of matrix and subsetting
#To get the second element
A[2]
B[2]
#To get second element of first row
A[1,2] #In the square braces, left of comma belongs to rows and right of comma represents columns
B[1,2]

#To extract the second row
A[2,]

#To extract the second column
A[,2]

#We can also label the rows and columns of a matrix using the function dimnames
dimnames(A)<-list(c("R1","R2","R3"),c("C1","C2"))
A

#### Operations on Matrices
# "+" addition 
# "-" subtraction
# "*" is element wise multiplication 
# "%*%" is matrix multiplication


#### Some frequently used functions
# cbind to bind the matrices/data frames by columns
# rbind to bind the matrices/data frames by rows
#Lets create two vectors and bind them to create matrix
A<-1:5
B<-6:10
cbind(A,B)
rbind(A,B)

##To find the dimensions of the matrix we use function "dim"
dim(cbind(A,B))
dim(rbind(A,B))

#Lets change the 5th element of vecto A to "Alpha" and see what happens
A
A[5]<-"Alpha"
A
A<-1:5
B<-6:10
mat<-cbind(A,B)
mat
mat[5]<-"Alpha"
mat

### DataFrames
# Data in the form of a matrix (rows and columns)
# The columns can be of different data types and type coersion doesn't happen
# This is important because the data we work on contain several attributes of different data types and it is essential to preserve them

#Lets create a simple data frame from vectors
A<-c(10,20,30,40)
B<-c("Alpha","Beta","Gamma","Delta")
C<-c(T,T,F,F)

DF<-data.frame(A,B,C)
DF
#### Some common functions on dataframes
## To understand the structure of dataframe
str(DF)

#To look into the summary of the dataframe
summary(DF)

#To access a column by name we use "$"
DF$A #The functions str, summary also work on individual columns

str(DF$A)

##### Some common functions
#To obtain maximum value, we use max
A<-c(14,-3,-99,88,7)
max(A)

# To obtain a minimum value, we use min
min(A)

# To get the index
#Eg which position of vector has max value
which.max(A)
#Similarly for min
which.min(A)

#Indices of all those positions that are less than 30
which(A<30,arr.ind = T)

#While creating a vector, if we want to create randomly, we use a function sample
A<-sample(x = 1:10,size = 20,replace = T)
A
#A function to get the size of a vector-length
length(A)

# In some cases, as in sample with replace=T, the elemets may repeat. so to
#get the unique elements
unique(A)
#To get how many unique of A
length(unique(A))
#We have a dataframe DF and we want to know the dimensions of it
dim(DF)

#To get the number of rows and number of columns of DF-nrow and ncol respectively
nrow(DF)
ncol(DF)

#################### R environment ####################
ls()
rm(list=c("A","B"))
ls()
rm(list=ls())

help(mean) # or ?mean

setwd("workingdirectory path")
getwd()

#install.packages(c("", ""))
#installed.packages()

############################## Read and write a file in R ##############################
rm(list=ls(all=T)) #TO remove the objects stored in workspace
getwd() # To check in which direcory we are presently in
# To set a directory
# To set working directory 
setwd("~/DataBox/DMaheshkumar/Rennes/CSE7212c/Day01/20180917_Batch 50_Rennes_MSc_CSE7212c_Lab01_R_Introduction")

#To read and write a file in R
#csv
Data<-read.csv("SampleData.csv",header=T,sep=",")
Data

#txt and tsv
Data<-read.table("SampleData.txt",header=T,sep="\t") 
write.table(Data,"Data2.txt",row.names=F,sep="\t") #check without row.names argument
write.table(Data,"Data3.tsv",row.names=F) #check without row.names argument
A<-read.table("Data3.tsv",header=T)
A
