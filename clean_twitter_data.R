library(tidyverse)
# set working directory
setwd('/Users/patrickschulze/Desktop/Consulting/Bundestag-MP-Analyse')

# ----------------------------------------------------------------------------------------------
# ----------------------------------- Aggregate Twitter data ----------------------------------- 
# ----------------------------------------------------------------------------------------------

# load data
tweepy_df <- read_delim('../programming/tweepy_df_test.csv', delim =',')
# inspect parsing problems
problems(tweepy_df)
# remove rows where problems occur, drop retweets and users where download failed,
# keep only relevant columns and rename columns (capitalized + in german language)
(tweepy_df <- tweepy_df[-problems(tweepy_df)$row,] %>% 
    filter(available == TRUE, is_retweet == 0) %>% 
    select(c('name','username','full_text','followers_count')) %>% 
    rename(Name = name, Twitter_Username = username, 
           Tweets = full_text, Anzahl_Follower = followers_count))
# aggregate data, i.e. concatenate all tweets of each person
(tweepy_docs_df_test <- tweepy_df %>% group_by(Name) %>% 
  mutate(Tweets_Dokument = paste(Tweets, collapse = ' ')) %>%
  summarize(Twitter_Username = max(Twitter_Username), Tweets_Dokument = max(Tweets_Dokument),
            Anzahl_Follower = max(Anzahl_Follower)))
# save aggregated data
# write.table(tweepy_docs_df_test, file = 'tweepy_docs_df_test.csv', row.names = FALSE)

# ----------------------------------------------------------------------------------------------
# ---------------------- Load and merge abg_df and se_df to Twitter data -----------------------
# ----------------------------------------------------------------------------------------------

# load data
abg_df <- read_delim('abg_df.csv', delim =',') %>%
  rename(Twitter_Username = Twitter, Wahlkreis_Nr = `Wahlkreis-Nr.`)
se_df <- read_delim('se_df.csv', delim =',') %>% 
  rename(Wahlkreis_Nr = `Wahlkreis-Nr.`) %>% select(-Bundesland)
# merge data
all_data <- tweepy_docs_df_test %>% inner_join(abg_df) %>% left_join(se_df, by = "Wahlkreis_Nr")
# drop variables that are completely expressed by other variables (e.g. Bevölkerung Deutsche 
# expressed by Bevölkerung mit Migratioshintergrund), and variables that are not easily 
# exploitable (e.g. Biografie) or uninformative (e.g. Fußnoten)
drop_vars <- c("Ausschuesse","Biografie","Land","Wahlkreis-Name","Bevölkerung am 31.12.2015 - Deutsche (in 1000)", 
                "Zensus 2011, Bevölkerung nach Migrationshintergrund am 09.05.2011 - ohne Migrationshintergrund (%)",
                "Fußnoten", "Bundesland-Nr.", "CDU", "SPD", "Die Linke", "Bündnis 90/Die Grünen",
                "CSU", "FDP", "AFD", "CDU/CSU")
all_data <- all_data %>% select(-drop_vars)

# ----------------------------------------------------------------------------------------------
# ---------------------- Preprocessing of documents with the stm package -----------------------
# ----------------------------------------------------------------------------------------------
library(stm)
library(tm)
# perform stemming (reduce words to their root form), drop punctuation and remove stop words
processed <- textProcessor(all_data$Tweets_Dokument, metadata = all_data[,-3])
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
