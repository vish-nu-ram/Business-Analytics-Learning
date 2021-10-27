data("iris")
iris
View(iris)
library(tidyverse)

#view data as a table
tbl_df(iris)

glimpse(iris)
gather(iris,"Sepal.Length","Sepal.Width")


#filtering data
filter(iris, Sepal.Length > 7)

#distinct rows
distinct(iris)

#Select some fraction of data
sample_frac(iris, 0.5, replace = TRUE)
sample_frac(iris, 0.5, replace = FALSE)

#select some random number of observations
sample_n(iris, 10, replace = TRUE)

#slicing some part of the data
slice(iris, 100:115)

#selecting columns based on conditions
select(iris, contains("."))
select(iris, ends_with("Length"))
select(iris, starts_with("Sepal"))


#arrange rows
arrange(iris, Sepal.Length)
arrange(iris, desc(Sepal.Length))

#summarise data
summarise(iris, avg = mean(Sepal.Length))
summarise_each(iris, funs(mean))
count(iris, Species, wt = Sepal.Length)

#Mutate
mutate(iris, sepal = Sepal.Length + Sepal.Width)

transmute(iris, sepal = Sepal.Length + Sepal.Width)


#group data
group_by(iris,Species)

iris %>% group_by(Species) %>% summarise(avg = mean(Sepal.Length))

X1=c("A","B","C")
X2=c(2,3,4)
a=data.frame(X1,X2)
a


X1=c("A","B","d")
X3=c("Y","o","u")
b=data.frame(X1,X3)
b

left_join(a, b, by = "X1")

right_join(a, b, by = "X1")
inner_join(a, b, by = "X1")
full_join(a, b, by = "X1")

