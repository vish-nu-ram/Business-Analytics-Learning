library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming
library(officer)
library(pdftools)
library(textmineR)
library(dplyr)
library(RCurl)
library(ROAuth)
library(plyr)
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(sentimentr)
library(syuzhet)
library(koRpus)
library(koRpus.lang.en)
library(twitteR)
library(stringi)



#reading text
txt<-c(""I'm in love with you," he said quietly.
        "Augustus," I said.
        "I am," he said. He was staring at me, and I could see the corners of his
        eyes crinkling. "I'm in love with you, and I'm not in the business of denying
        myself the simple pleasure of saying true things. I'm in love with you, and I
        know that love is just a shout into the void, and that oblivion is inevitable,
        and that we're all doomed and that there will come a day when all our labor
        has been returned to dust, and I know the sun will swallow the only earth
        we'll ever have, and I am in love with you."
        "Augustus," I said again, not knowing what else to say. It felt like
        everything was rising up in me, like I was drowning in this weirdly painful
        joy, but I couldn't say it back. I couldn't say anything back. I just looked at
        him and let him look at me until he nodded, lips pursed, and turned away,
        placing the side of his head against the window.")
#length of the text 
lengths(strsplit(txt, "\\W+"))


#detecting a character or pattern in the given text or a string.
str_detect(txt,"Augustus")

#counting a certain pattern or string
str_count(txt,"love")

#locating a certain string 
str_locate(txt,"love")
str_locate_all(txt,"love")
str_locate_all(txt,"ab")

#length of string 
str_length(txt)

#replacing a pattern
txt_LOVE<-str_replace(txt,"love","LOVE")
txt_LOVE<-str_replace_all(txt,"love","LOVE")


#patterns to identify multiple texts 

#or
str_count(txt,"[wj]")
#one of them
str_count(txt,"in|you")
#none of them
str_count(txt,"[^wj]")


#splitting based on a pattern
txt2<-strsplit(txt,"\n")
txt2
#handling each element
txt2[[1]][2]

strsplit(txt,"and")


stri_count_fixed(txt, "love")
stri_count_fixed(txt,"augustus")

#multiple arguments
stri_count_fixed(txt,c("love","augustus"))

# Make lowercase
tolower(txt)

# Remove punctuation
removePunctuation(txt)

# Remove numbers
removeNumbers(txt)

# Remove whitespace
stripWhitespace(txt)


removeWords(txt, stopwords("en"))

pal<- brewer.pal(8,"Dark2")

wordcloud(txt, min.freq = 1, max.words = Inf, width=1000,
          height=1000, random.order = FALSE, color= pal )


mysentiment<- get_nrc_sentiment(txt)

library(SentimentAnalysis)
sentiment <- analyzeSentiment(txt)
sentiment

