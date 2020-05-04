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

# build document-feature matrix, where a feature corresponds to a word in our case
# stopwords, numbers and punctuation are removed and word stemming is performed
# each word is indexed and frequency per document assigned
stopwords_de <- 
  read_lines("https://raw.githubusercontent.com/stopwords-iso/stopwords-de/master/stopwords-de.txt")

length(stopwords_de) # 620 stopwords
length(stopwords('de')) # 231 stopwords
c(stopwords_de, stopwords('de')) %>% unique() %>% length() #> 620

stopwords_customized <- setdiff(stopwords_de, stopwords('de'))
# amp from '&' and innen from -innen
stopwords_customized <- c(stopwords_customized, 'amp', 'innen')

# build document-feature matrix
dfmatrix <- quanteda::dfm(corp_all_data, 
                remove = c(stopwords('de'),
                           stopwords('en'),
                           stopwords_customized,
                           min_nchar = 2),
                remove_numbers = TRUE,
                remove_punct = TRUE,
                remove_url = TRUE,
                stem = TRUE,
                tolower = TRUE,
                verbose = FALSE)

quanteda::topfeatures(dfmatrix, 20) # check most frequent words

# remove # from hashtags
dfmatrix@Dimnames$features <- gsub("#", "", dfmatrix@Dimnames$features)

# remove emojis and @username strings
emojis <- paste0("[\U{1f300}-\U{1f5ff}\U{1f900}-\U{1f9ff}\U{1f600}-\U{1f64f}\U{1f680}-",
                 "\U{1f6ff}\U{2600}-\U{26ff}\U{2700}-\U{27bf}\U{1f1e6}-\U{1f1ff}\U{1f191}-",
                 "\U{1f251}\U{1f004}\U{1f0cf}\U{1f170}-\U{1f171}\U{1f17e}-\U{1f17f}\U{1f18e}",
                 "\U{3030}\U{2b50}\U{2b55}\U{2934}-\U{2935}\U{2b05}-\U{2b07}\U{2b1b}-\U{2b1c}",
                 "\U{3297}\U{3299}\U{303d}\U{00a9}\U{00ae}\U{2122}\U{23f3}\U{24c2}\U{23e9}-",
                 "\U{23ef}\U{25b6}\U{23f8}-\U{23fa}]")
# indices of emojis in dfm
emojis_idx <- grep(emojis,dfmatrix@Dimnames$features)
# indices of @username
usr_idx <- grep("@",dfmatrix@Dimnames$features)
# remove these fields
dfmatrix@Dimnames$features <- dfmatrix@Dimnames$features[-union(emojis_idx, usr_idx)]

# check most frequent words again
quanteda::topfeatures(dfmatrix, 20)

# remove all words that appear in less than 2 documents or 1% across all documents
dfmatrix <- quanteda::dfm_trim(dfmatrix, min_termfreq = 5, min_docfreq = 3)

# convert to stm object (this reduces memory use when fitting stm; see ?stm)
twitter_preprocessed <- quanteda::convert(dfmatrix, to = "stm")

# extract documents, vocabulary and metadata
docs <- twitter_preprocessed$documents
vocab <-  twitter_preprocessed$vocab
meta <- twitter_preprocessed$meta

# ----------------------------------------------------------------------------------------------
# ------------------------------------ Fitting the stm -----------------------------------------
# ----------------------------------------------------------------------------------------------

mod <- stm::stm(documents = docs, vocab = vocab, K=20, prevalence =~ Partei,
           max.em.its = 75, data = meta, init.type = "Spectral")
plot(mod, type = "summary", xlim = c(0, .3))
