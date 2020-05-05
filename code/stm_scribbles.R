# ----------------------------------------------------------------------------------------------
# Preparation -------------------------------------------------------------
# ----------------------------------------------------------------------------------------------

# Install and load required packages
os <- Sys.info()[["sysname"]] # Get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # Set corresponding installation type
packages_required <- c(
  "dplyr","ggplot2", "quanteda", "stm", "tidyverse", "tm"  
)
not_installed <- packages_required[!packages_required %in%
                                     installed.packages()[, "Package"]]
if (length(not_installed) > 0) {
  lapply(
    not_installed,
    install.packages,
    repos = "http://cran.us.r-project.org",
    dependencies = TRUE,
    type = itype
  )
}
lapply(packages_required, library, quietly = TRUE, character.only = TRUE)

# set working directory
setwd('C:\\Users\\Simon\\Desktop\\Twitter')

# ----------------------------------------------------------------------------------------------
# ----------------------------------- Aggregate Twitter data ----------------------------------- 
# ----------------------------------------------------------------------------------------------

# load data
tweepy_df <- read_delim('tweepy_df.csv', delim = ',')
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
(tweepy_docs_df <- tweepy_df %>% group_by(Name) %>% 
  mutate(Tweets_Dokument = paste(Tweets, collapse = ' ')) %>%
  summarize(Twitter_Username = max(Twitter_Username), Tweets_Dokument = max(Tweets_Dokument),
            Anzahl_Follower = max(Anzahl_Follower)))
# save aggregated data
saveRDS(tweepy_docs_df, "tweepy_docs_df.rds")
# write.table(tweepy_docs_df_test, file = 'tweepy_docs_df_test.csv', row.names = FALSE)

# ----------------------------------------------------------------------------------------------
# ---------------------- Load and merge abg_df and se_df to Twitter data -----------------------
# ----------------------------------------------------------------------------------------------

# load data
tweepy_docs_df <- readRDS("tweepy_docs_df.rds")

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
# ---------------------- Preprocessing of documents with the quanteda package ------------------
# ----------------------------------------------------------------------------------------------

# creating corpus
corp_all_data <- quanteda::corpus(x = all_data, text_field = "Tweets_Dokument", docid_field = "Name")
summary(corp_all_data, 5)

 
# # creating tokens
# toks <- tokens(corp_all_data, remove_punct = TRUE)
# toks_nostop <- tokens_select(toks, pattern = stopwords('de'), selection = 'remove')
# dfmatrix <- dfm(toks_nostop)

# dfm stopwords
stopwords_de <- 
  read_lines("https://raw.githubusercontent.com/stopwords-iso/stopwords-de/master/stopwords-de.txt")

length(stopwords_de) # 620 stopwords
length(stopwords('de')) # 231 stopwords
c(stopwords_de, stopwords('de')) %>% unique() %>% length() #> 620

stopwords_customized <- setdiff(stopwords_de, stopwords('de'))
stopwords_customized <- c(stopwords_customized, 'amp', 'innen') # amp from '&' and innen from -innen

dfmatrix <- quanteda::dfm(corp_all_data, 
                remove = c(stopwords('de'),
                           stopwords('en'),
                           stopwords_customized,
                min_nchar = 4),
                # remove_hyphens = TRUE,
                remove_numbers = TRUE,
                remove_punct = TRUE,
                remove_symbols = TRUE,
                remove_url = TRUE,
                stem = TRUE,
                tolower = TRUE,
                verbose = FALSE)

quanteda::topfeatures(dfmatrix, 100) # check most frequent words

# remove # from hashtags
dfmatrix@Dimnames$features <- gsub("#", "", dfmatrix@Dimnames$features)

# # remove emojis and @username strings
# emojis <- paste0("[\U{1f300}-\U{1f5ff}\U{1f900}-\U{1f9ff}\U{1f600}-\U{1f64f}\U{1f680}-",
#                  "\U{1f6ff}\U{2600}-\U{26ff}\U{2700}-\U{27bf}\U{1f1e6}-\U{1f1ff}\U{1f191}-",
#                  "\U{1f251}\U{1f004}\U{1f0cf}\U{1f170}-\U{1f171}\U{1f17e}-\U{1f17f}\U{1f18e}",
#                  "\U{3030}\U{2b50}\U{2b55}\U{2934}-\U{2935}\U{2b05}-\U{2b07}\U{2b1b}-\U{2b1c}",
#                  "\U{3297}\U{3299}\U{303d}\U{00a9}\U{00ae}\U{2122}\U{23f3}\U{24c2}\U{23e9}-",
#                  "\U{23ef}\U{25b6}\U{23f8}-\U{23fa}]")
# # indices of emojis in dfm
# emojis_idx <- grep(emojis,dfmatrix@Dimnames$features)
# # remove these fields
# dfmatrix@Dimnames$features <- dfmatrix@Dimnames$features[-union(emojis_idx, usr_idx)]

# indices of @username
usr_idx <- grep("@",dfmatrix@Dimnames$features)

dfmatrix@Dimnames$features <- dfmatrix@Dimnames$features[-usr_idx]

# # based on tfidf frequencies
# dfmatrix_tfidf <- quanteda::dfm_tfidf(dfmatrix)

# select features of dfm
dfmatrix <- quanteda::dfm_trim(dfmatrix, min_termfreq = 1, min_docfreq = 0.01)

# convert to stm object (this reduces memory use when fitting stm; see ?stm)
twitter_preprocessed <- quanteda::convert(dfmatrix, to = "stm")

# extract documents and vocabulary
docs <- twitter_preprocessed$documents
vocab <-  twitter_preprocessed$vocab

# metadata is best assigned separately in the end
meta <- all_data[,-3]

# ----------------------------------------------------------------------------------------------
# ---------------------- STM -----------------------
# ----------------------------------------------------------------------------------------------

# prevalence model
mod <- stm::stm(documents = docs, vocab = vocab, K=10, prevalence =~ Partei,
           max.em.its = 75, data = meta, init.type = "Spectral")

# top words per topic
labelTopics(mod, c(1, 2, 3))

thoughts3 <- stm::findThoughts(mod, texts = docs, n = 2,
                          topics = 3)$docs[[1]]
thoughts20 <- stm::findThoughts(poliblogPrevFit, texts = shortdoc, n = 2,
                           topics = 18)$docs[[1]]
par(mfrow = c(1, 2), mar = c(0.5, 0.5, 1, 0.5))
plotQuote(thoughts3, width = 30, main = "Topic 6")
plotQuote(thoughts20, width = 30, main = "Topic 18")

# estimating metadata/topic relationship
meta$Partei <- as.factor(meta$Partei)
prep <- stm::estimateEffect(1:10 ~ Partei, mod,
                       metadata = meta, uncertainty = "Global")
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
plot(prep, "Partei", method = "pointestimate", topics = 6,
     model = mod, printlegend = FALSE, xaxt = "n", xlab = "Partei")
monthseq <- seq(from = as.Date("2008-01-01"),
                to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),
     labels = monthnames)

# topical content
mod_topic <- stm::stm(documents = docs, vocab = vocab, K = 10,
                       prevalence =~ Partei, content =~ Partei,
                       max.em.its = 75, data = meta, init.type = "Spectral")

plot(mod_topic, type = "perspectives", topics = 10)
plot(mod, type = "perspectives", topics = c(8, 10))
