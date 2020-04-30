library(tidyverse)
# set working directory
setwd('/Users/patrickschulze/Desktop/Consulting/Bundestag-MP-Analyse')
# ----------------------------------------------------------------------------------------------
# load data
tweepy_df <- read_delim('../programming/tweepy_df_test.csv', delim =',')
# inspect parsing problems
problems(tweepy_df)
# remove rows where problems occur,
# drop retweets and users where download failed,
# and keep only relevant columns
(tweepy_df <- tweepy_df[-problems(tweepy_df)$row,] %>% 
    filter(available == TRUE, is_retweet == 0) %>% 
    select(c('name','username','full_text','followers_count')))
# aggregate data, i.e. concatenate all tweets of each person
(tweepy_docs_df_test <- tweepy_df %>% group_by(name) %>% 
  mutate(all_tweets = paste(full_text, collapse = ' ')) %>%
  summarize(username = max(username), all_tweets = max(all_tweets),
  followers_count = max(followers_count)))
# save aggregated data
write.table(tweepy_docs_df_test, file = 'tweepy_docs_df_test.csv', row.names = FALSE)
# ----------------------------------------------------------------------------------------------
# preprocessing of documents with the stm package
library(stm)
library(tm)
# perform stemming (reduce words to their root form), drop punctuation and remove stop words
processed <- textProcessor(tweepy_docs_df_test$all_tweets, metadata = tweepy_docs_df_test)
str(processed)
# drop all words below word threshold. if e.g. word threshold is 1 (default), all words
# that appear less than 1 time in every document are dropped
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
# for every document, freqeuency for each word that word appears in this document
docs <- out$documents
# actual words that the indices in docs represent
vocab <- out$vocab
# metadata
meta <- out$meta
# ----------------------------------------------------------------------------------------------
# preprocessing of documents with the quanteda package
library(quanteda)
