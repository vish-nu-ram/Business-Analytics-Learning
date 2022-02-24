library(dplyr)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams
austen_bigrams %>% count(bigram, sort = TRUE)


# filter stop words

library(tidyr)

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)


#bigram in sentiment analysis

install.packages("textdata")
library(textdata)
AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)



library(ggplot2)

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")


#network graph
#install.packages("igraph")
library(igraph)

# original counts
bigram_counts

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

#install.packages("ggraph")
library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


#Correlation and Phi

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

install.packages("widyr")
library(widyr)

# count words co-occuring within sections
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)


word_pairs %>%
  filter(item1 == "darcy")


#Phi

word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors %>%
  filter(item1 == "pounds")

word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

set.seed(2016)

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()



# Naive Bayesian

library(tidyverse)
library(ggplot2)
library(caret)
#install.packages("caretEnsemble")
library(caretEnsemble)
#install.packages("psych")
library(psych)
#install.packages("Amelia")
library(Amelia)
library(mice)
#install.packages("GGally")
library(GGally)
library(rpart)
library(randomForest)

data<- read.csv("diabetes.csv")

data$Outcome <- factor(data$Outcome, levels = c(0,1), labels = c("False", "True"))

head(data)


#Convert '0' values into NA
data[, 2:7][data[, 2:7] == 0] <- NA

#visualize the missing data
missmap(data)

# remove NAs
library(mice)
mice_mod <- mice(data[, c("Glucose","BloodPressure","SkinThickness","Insulin","BMI")], method='rf')
mice_complete <- complete(mice_mod)


data$Glucose <- mice_complete$Glucose
data$BloodPressure <- mice_complete$BloodPressure
data$SkinThickness <- mice_complete$SkinThickness
data$Insulin<- mice_complete$Insulin
data$BMI <- mice_complete$BMI

missmap(data)

#ggpally package
ggpairs(data)


#Building a model
#split data into training and test data sets
indxTrain <- createDataPartition(y = data$Outcome,p = 0.75,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,] 
#Check dimensions of the split > 
prop.table(table(data$Outcome)) * 100
prop.table(table(training$Outcome)) * 100
prop.table(table(testing$Outcome)) * 100

x = training[,-9]
y = training$Outcome

#classification
library(e1071)
# list of methods http://topepo.github.io/caret/train-models-by-tag.html
model = train(x,y,method='nb',trControl=trainControl(method='cv',number=10))



#Model Evaluation
#Predict testing set
Predict <- predict(model,newdata = testing ) 
confusionMatrix(data=Predict, reference = testing$Outcome)






# Twitter 
library(twitteR)

consumer_key <- ''
consumer_secret <- ''
access_token <- ''
access_secret <- ''

#bearer_token<-'xxxx'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

virus <- searchTwitter('#Covid19 + #Coronavirus', lang='en', n = 1000, since = '2020-01-01', retryOnRateLimit = 1e3)
virus_df = twListToDF(virus)


# cleaning and processing twitter data

data_tm<-virus_df %>% select(text,id)


#clean data
data_tm$text <- sub("RT.*:", "", data_tm$text)
data_tm$text <- sub("@.* ", "", data_tm$text)

text_cleaning_tokens <- data_tm %>% tidytext::unnest_tokens(word, text)

text_cleaning_tokens$word <- gsub('[[:digit:]]+', '', text_cleaning_tokens$word)
text_cleaning_tokens$word <- gsub('[[:punct:]]+', '', text_cleaning_tokens$word)
text_cleaning_tokens <- text_cleaning_tokens %>% filter(!(nchar(word) == 1))%>% 
  anti_join(stop_words)

tokens <- text_cleaning_tokens %>% filter(!(word==""))
tokens <- tokens %>% mutate(ind = row_number())
tokens <- tokens %>% group_by(id) %>% mutate(ind = row_number()) %>%
  tidyr::spread(key = ind, value = word)

tokens [is.na(tokens)] <- ""
tokens <- tidyr::unite(tokens, text,-id,sep =" " )
tokens$text <- trimws(tokens$text)


# LDA Topic Models


library(tm)
library(topicmodels)
library(textmineR)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidytext)



data("AssociatedPress")
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_topics <- tidy(ap_lda, matrix = "beta")
AssociatedPress


ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents
