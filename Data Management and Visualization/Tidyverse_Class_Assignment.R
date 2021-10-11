library(tidyverse)
library(readxl)

manhattan_data <- read_excel("rollingsales_manhattan.xlsx", skip = 4)
manhattan_data
glimpse(manhattan_data)
# str(manhattan_data)

typeof(manhattan_data$BOROUGH)
typeof(manhattan_data)

#  Pipe operator %>% is used to pass an object to a function

manhattan_data %>% glimpse()

distinct(manhattan_data)

library(ggplot2)

mpg

ggplot(mpg) + 
geom_point(mapping = aes(x= displ, y = hwy))

ggplot(mpg) +
  geom_point( mapping = aes(x= displ, y = hwy))+
  facet_wrap(~class, nrow = 2)

ggplot(mpg) +
  geom_point( mapping = aes(x= displ, y = hwy))+
  facet_wrap(~manufacturer, nrow = 2)

ggplot(mpg) +
  geom_point( mapping = aes(x= displ, y = hwy))+
  facet_wrap(drv ~ cyl)

ggplot(mpg) +
  geom_smooth( mapping = aes(x= displ, y = hwy, linetype = drv))

ggplot(mpg) +
  geom_smooth( mapping = aes(x= displ, y = hwy, color = drv))