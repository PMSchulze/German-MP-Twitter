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
