library(tidyverse)
# set working directory
setwd('C:\\Users\\Simon\\Desktop\\Twitter')

# ----------------------------------------------------------------------------------------------
# ----------------------------------- Aggregate Twitter data ----------------------------------- 
# ----------------------------------------------------------------------------------------------

# load data
tweepy_df <- read_delim('tweepy_df_test.csv', delim =',')
# inspect parsing problems
problems(tweepy_df)
# remove rows where problems occur, drop retweets and users where download failed,
# keep only relevant columns and rename columns (capitalized + in german language)
(tweepy_df <- tweepy_df %>% 
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
plotRemoved(processed$documents, lower.thresh = seq(0, 10, by = 1))
out <- prepDocuments(processed$documents, processed$vocab,
                     processed$meta, lower.thresh = 1)

# for every document, freqeuency for each word that word appears in this document
docs <- out$documents
# actual words that the indices in docs represent
vocab <- out$vocab
# metadata
meta <- out$meta
# ----------------------------------------------------------------------------------------------
# ---------------------- Preprocessing of documents with the quanteda package -----------------------
# ----------------------------------------------------------------------------------------------
library(quanteda)
library(readtext)
library(ggplot2)

# creating corpus
corp_all_data <- corpus(x = all_data, text_field = "Tweets_Dokument", docid_field = "Name")
print(corp_all_data)

summary(corp_all_data, 5)

# extracting document variables
docvars(corp_all_data, field = "Geburtsjahr")
corp_all_data$Geburtsjahr

# creating tokens
toks <- tokens(corp_all_data, remove_punct = TRUE)

toks_nostop <- tokens_select(toks, pattern = stopwords('de'), selection = 'remove')

# dfm
dfmatrix <- dfm(toks_nostop)

# get most frequent features
topfeatures(dfmatrix, 10)

# dfm stopwords
stopwords_de <- 
  read_lines("https://raw.githubusercontent.com/stopwords-iso/stopwords-de/master/stopwords-de.txt")

length(stopwords_de) # 620 stopwords
length(stopwords('de')) # 231 stopwords
c(stopwords_de, stopwords('de')) %>% unique() %>% length() #> 620

stopwords_customized <- setdiff(stopwords_de, stopwords('de'))
stopwords_customized <- c(stopwords_customized, 'amp', 'innen') # amp from '&' and innen from -innen
# still to exclude: \U001f...

dfmatrix <- dfm(corp_all_data, 
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

topfeatures(dfmatrix, 20) # check most frequent words

dfmatrix_tfidf <- dfm_tfidf(dfmatrix) # based on tfidf frequencies

# select features of dfm
dfmatrix <- dfm_trim(dfmatrix, min_termfreq = 1, min_docfreq = 0.01)

# convert to stm object (this reduces memory use when fitting stm; see ?stm)
twitter_preprocessed <- convert(dfmatrix, to = "stm")

# extract documents and vocabulary
docs <- twitter_preprocessed$documents
vocab <-  twitter_preprocessed$vocab

# metadata is best assigned separately in the end
meta <- all_data[,-3]

# ----------------------------------------------------------------------------------------------
# ---------------------- STM -----------------------
# ----------------------------------------------------------------------------------------------

# prevalence model
mod <- stm(documents = docs, vocab = vocab, K=20, prevalence =~ Partei,
           max.em.its = 75, data = meta, init.type = "Spectral")


# prevalence model
poliblogPrevFit <- stm(documents = out$documents, vocab = out$vocab, 
                       K = 10, prevalence =~ Partei, max.em.its = 75,
                       data = out$meta, init.type = "Spectral")

# top words per topic
labelTopics(mod, c(1, 2, 3))

thoughts3 <- findThoughts(poliblogPrevFit, texts = shortdoc, n = 2,
                          topics = 6)$docs[[1]]
thoughts20 <- findThoughts(poliblogPrevFit, texts = shortdoc, n = 2,
                           topics = 18)$docs[[1]]
par(mfrow = c(1, 2), mar = c(0.5, 0.5, 1, 0.5))
plotQuote(thoughts3, width = 30, main = "Topic 6")
plotQuote(thoughts20, width = 30, main = "Topic 18")

# estimating metadata/topic relationship
out$meta$Partei <- as.factor(out$meta$Partei)
prep <- estimateEffect(1:10 ~ Partei, poliblogPrevFit,
                       metadata = out$meta, uncertainty = "Global")
summary(prep, topics = 3)

# summary visualization
plot(mod, type = "summary", xlim = c(0, 0.3))

# only for a binary covariate
plot(prep, covariate = "rating", topics = c(6, 13, 18),
     model = poliblogPrevFit, method = "difference", cov.value1 = "Liberal",
     cov.value2 = "Conservative",
     xlab = "More Conservative ... More Liberal",
     main = "Effect of Liberal vs. Conservative", xlim = c(-0.1, 0.1),
     labeltype = "custom", custom.labels = c("Obama/McCain", "Sarah Palin",
                                             "Bush Presidency"))

# metadata/topic relationship visualization
plot(prep, "day", method = "continuous", topics = 13,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Time (2008)")
monthseq <- seq(from = as.Date("2008-01-01"),
                to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),
     labels = monthnames)

# topical content
poliblogContent <- stm(out$documents, out$vocab, K = 10,
                       prevalence =~ Partei, content =~ Partei,
                       max.em.its = 75, data = out$meta, init.type = "Spectral")

plot(poliblogContent, type = "perspectives", topics = 10)
plot(poliblogPrevFit, type = "perspectives", topics = c(8, 10))
