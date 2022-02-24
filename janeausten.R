#loading libraries
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(wordcloud)




#reading data
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

#unnesting tokens 
tidy_books <- original_books %>%
  unnest_tokens(word, text)


#removing stopwords
data(stop_words)


tidy_books <- tidy_books %>%
  anti_join(stop_words)

#most frequent words
tidy_books %>%
  count(word, sort = TRUE)

#visualizing the most common words
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#getting sentiments into the environment 
nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")
1


#finding the joyful words from Emma
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)


#getting sentiments
janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#plotting sentiment
ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")



#pride and prejudice book
pride_prejudice <- tidy_books %>%
  filter(book == "Pride & Prejudice")


#sentiments using affin
afinn <- pride_prejudice %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")
1


bing_and_nrc <- bind_rows(
  pride_prejudice %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive",
                                         "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


          bing_and_nrc %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

          
get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive","negative")) %>%
            count(sentiment)


tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


