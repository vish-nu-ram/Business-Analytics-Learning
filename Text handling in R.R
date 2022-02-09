

c = paste(c('B','U','1234'), collapse = '|')
c

d = paste0('B','U','1234')
d
strsplit("A is a char"," ")
substr("Red fox jumped over xyz",9,18)
library(dplyr)
library(tidyverse)
install.packages("tidytext")
library(tidytext)

text <- c("this is a sentance 1 for asd -",
          "He kindly said hello world -",
          "The carriage is also a random sentance -",
          "and immortality"
          )

text_df <- text %>% tibble(line=1:4)
text_df <- tibble(line = 1:4, text = text)
text_df

token1 <- text_df%>%unnest_tokens(words,text)
token1

token2 <- text_df%>%unnest_tokens(words,text, token = "ngrams", n=2)
token2

table(token2$line)

install.packages("gutenbergr")
library(gutenbergr)
library(urltools)
library(Rcpp)

data("stop_words")

hgwells<- gutenberg_download(c(35,36,5230,159))

tidy_hgwells <- hgwells %>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)

tidy_hgwells

tidy_hgwells %>%
  count(word, sort= TRUE)
