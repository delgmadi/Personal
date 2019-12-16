library(mallet)
library(dplyr)
library(tidytext)
library(ggplot2)

#This will show how to make dendrographs + find topic models

#First - we need to get some stopwords (writelines is self generating code)
tmp <- tempfile()
writeLines(stop_words$word, tmp)

#then I'm going to create a collection of words. I'm going to use the new Harry Styles Album
FineLine <- genius_album(artist= "Harry Styles", album = "Fine Line")
View(FineLine)

#here is where we really start to do some interseting stuff
#this code calls for a mallet import of the Harry Styles data
#the first argument is the name of each document, which we can assume is the text
#the second argument is the text of the paragraph
#the third argument is calling for the stopwords file (label, text, stop words)
docs <- mallet.import(FineLine$lyric, FineLine$lyric, tmp)

# create and run a topic model
#this will tell the model to make five topics, this is an arbitrary number
#you can make as many topics as you have ram
topic_model <- MalletLDA(num.topics = 5)
#the model then loads the docs we specified above
topic_model$loadDocuments(docs)
#lets train the model 2000 times.
topic_model$train(2000)


# tidy the word-topic combinations
td_beta <- tidy(topic_model)
td_beta

# Examine the five topics
td_beta %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta)) +
  geom_col() +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#at this point you can start looking for how the categories differ
#what is in 3 that isn't in 4, for instance

#this produces a nice table with all the probabilites
#super useful if you want to JOIN these back to other details
doc.topics<-mallet.doc.topics(topic_model, normalized = TRUE, smoothed = TRUE)

#these won't make sense to you, but that is ok
topic.words<-mallet.topic.words(topic_model, normalized = TRUE, smoothed = TRUE)

#This is a dendrogram (like a family tree) of how the topics relate to each other
plot(mallet.topic.hclust(doc.topics, topic.words, balance = .4))

#these are the topic labels, which means the keys the process created to see the key contents
mallet.topic.labels(topic_model, topic.words, num.top.words = 3)



