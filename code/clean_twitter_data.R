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
tweepy_df <- tweepy_df[-problems(tweepy_df)$row,] %>% 
    filter(available == TRUE, is_retweet == 0) %>% 
    select(c('name','username','full_text','followers_count')) %>% 
    rename(Name = name, Twitter_Username = username, 
           Tweets = full_text, Anzahl_Follower = followers_count)
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
abg_df <- read_delim('./data/abg_df.csv', delim =',') %>%
  rename(Twitter_Username = Twitter, Wahlkreis_Nr = `Wahlkreis-Nr.`)
se_df <- read_delim('./data/se_df.csv', delim =',') %>% 
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
(all_data <- all_data %>% select(-drop_vars))

# ----------------------------------------------------------------------------------------------
# ------------------ Preprocessing of documents with the quanteda package ----------------------
# ----------------------------------------------------------------------------------------------

library(quanteda)
library(stm)
library(tm)

# build corpus, which by default organizes documents into types, tokens and sentences
corp_all_data <- quanteda::corpus(x = all_data, text_field = "Tweets_Dokument", 
                                  docid_field = "Name")
# convert some special german characters and remove # infront of hashtags
corp_text_cleaned <- stringi::stri_replace_all_fixed(
  texts(corp_all_data), 
  c("ä", "ö", "ü", "Ä", "Ö", "Ü", "ß", "#"), 
  c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss", ""), 
  vectorize_all = FALSE)
texts(corp_all_data) <- corp_text_cleaned

# build document-feature matrix, where a feature corresponds to a word in our case
# stopwords, numbers and punctuation are removed and word stemming is performed
# each word is indexed and frequency per document assigned

# start with defining stopwords
stopwords_de <- 
  read_lines("https://raw.githubusercontent.com/stopwords-iso/stopwords-de/master/stopwords-de.txt")
length(stopwords_de) # 620 stopwords
length(stopwords('de')) # 231 stopwords
# combine all stopwords (amp from '&' and innen from -innen)
stopwords_de_customized <- Reduce(union, list(stopwords_de, stopwords("de"), "amp", "innen"))
# convert special characters for stopwords (otherwise many stopwords are not detected!)
stopwords_de_customized <- stringi::stri_replace_all_fixed(
  stopwords_de_customized, 
  c("ä", "ö", "ü", "Ä", "Ö", "Ü", "ß"), 
  c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss"), 
  vectorize_all = FALSE)

# build document-feature matrix
dfmatrix <- quanteda::dfm(
  corp_all_data,
  remove = c(stopwords_de_customized,
             stopwords('en')),
  remove_hyphens = TRUE,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_url = TRUE,
  tolower = TRUE,
  verbose = FALSE)

# check most frequent words
quanteda::topfeatures(dfmatrix, 20)

# manually remove specific tokens and all tokens with <4 characters
dfmatrix_cleaned <- dfmatrix %>% 
  quanteda::dfm_remove(pattern = "@", min_nchar = 4, valuetype = "regex") %>% # @username
  quanteda::dfm_remove(pattern = "(^[0-9]+[,.][0-9]+)\\w{1,3}$", valuetype = "regex") %>% # 1000ter,...
  quanteda::dfm_remove(pattern = "^[^a-zA-Z0-9]*$", valuetype = "regex") %>% # non-alphanumerical
  quanteda::dfm_remove(pattern = "^jaehrig", valuetype = "regex") %>% #jaehrig
  quanteda::dfm_remove(pattern = "^.*(aaa|aeae|hhh).*$", valuetype = "regex") %>% # aaaawww,...
  quanteda::dfm_remove(pattern = "^http", valuetype = "regex") # http...
  
# perform word stemming and remove all words that appear rarely
dfmatrix_cleaned <- dfmatrix_cleaned %>% 
  quanteda::dfm_wordstem(language = "german") %>% 
  quanteda::dfm_trim(min_termfreq = 5, min_docfreq = 3)

# check most frequent words again
quanteda::topfeatures(dfmatrix_cleaned, 20)

# convert to stm object (this reduces memory use when fitting stm; see ?stm)
twitter_preprocessed <- quanteda::convert(dfmatrix_cleaned, to = "stm")

# extract documents, vocabulary and metadata
docs <- twitter_preprocessed$documents
vocab <-  twitter_preprocessed$vocab
meta <- twitter_preprocessed$meta

# ----------------------------------------------------------------------------------------------
# ------------------------------------ Fitting the stm -----------------------------------------
# ----------------------------------------------------------------------------------------------

mod <- stm::stm(documents = docs, vocab = vocab, K=10, prevalence =~ Partei,
           max.em.its = 85, data = meta, init.type = "Spectral")
plot(mod, type = "summary", xlim = c(0, .3))
